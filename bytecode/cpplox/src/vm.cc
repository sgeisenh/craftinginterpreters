#include "vm.h"

#include <cstdarg>
#include <cstdint>
#include <iostream>
#include <optional>

#include "absl/strings/str_format.h"
#include "compiler.h"
#include "value.h"

namespace {

Value ClockNative(int arg_count, Value *args) {
  return (double)clock() / CLOCKS_PER_SEC;
}

constexpr std::string_view kInit = "init";

} // namespace

VM::VM() {
  Obj::SetGc(new GarbageCollector(this));
  DefineNative("clock", ClockNative);
  init_string_ = intern_table_.CreateString(kInit);
  ObjFunction *function = new ObjFunction();
  Push(function);
}

VM::~VM() {
  init_string_ = nullptr;
  Obj::FreeObjects();
}

InterpretResult VM::Interpret(std::string_view source) {
  ObjFunction *function = new ObjFunction();
  Push(function);
  Compiler compiler(intern_table_, *function);
  compiler_ = &compiler;
  if (!compiler.Compile(source)) {
    // Move this into a scope guard?
    compiler_ = nullptr;
    return InterpretResult::COMPILE_ERROR;
  }
  ObjClosure *closure = new ObjClosure(function);
  Pop();
  Push(closure);
  Call(*closure, 0);
  compiler_ = nullptr;
  return Run();
}

void VM::Push(Value value) {
  *stack_top_ = std::move(value);
  ++stack_top_;
}

Value VM::Pop() {
  --stack_top_;
  return *stack_top_;
}

Value VM::Peek(int distance) { return stack_top_[-1 - distance]; }

bool VM::Call(ObjClosure &closure, int arg_count) {
  if (arg_count != closure.Function().Arity()) {
    RuntimeError(absl::StreamFormat("Expected %d arguments but got %d.",
                                    closure.Function().Arity(), arg_count));
    return false;
  }
  if (frame_count_ == kFramesMax) {
    RuntimeError("Stack overflow.");
    return false;
  }
  CallFrame &frame = frames_[frame_count_];
  ++frame_count_;
  frame.closure = &closure;
  frame.new_ip = closure.Function().GetChunk().Data();
  frame.slots = stack_top_ - arg_count - 1;
  return true;
}

bool VM::CallValue(Value callee, int arg_count) {
  Obj *obj = callee.AsObject();
  if (obj == nullptr) {
    RuntimeError("Can only call functions and classes.");
    return false;
  }
  switch (obj->GetType()) {
  case ObjType::BOUND_METHOD: {
    ObjBoundMethod *bound_method = obj->AsBoundMethod();
    stack_top_[-arg_count - 1] = bound_method->Receiver();
    return Call(bound_method->Method(), arg_count);
  }
  case ObjType::KLASS: {
    ObjKlass &klass = *obj->AsKlass();
    stack_top_[-arg_count - 1] = new ObjInstance(klass);
    if (auto *maybe_value = klass.Methods().at(init_string_);
        maybe_value != nullptr) {
      return Call(*maybe_value->AsClosure(), arg_count);
    } else if (arg_count != 0) {
      RuntimeError(
          absl::StreamFormat("Expected 0 arguments but got %d.", arg_count));
      return false;
    }
    return true;
  }
  case ObjType::CLOSURE: {
    return Call(*obj->AsClosure(), arg_count);
  }
  case ObjType::NATIVE: {
    Value result = obj->AsNative()->Call(arg_count, stack_top_ - arg_count);
    stack_top_ -= arg_count + 1;
    Push(result);
    return true;
  }
  default: {
    break;
  }
  }
  RuntimeError("Can only call functions and classes.");
  return false;
}

bool VM::InvokeFromClass(ObjKlass *klass, ObjString *name, int arg_count) {
  if (auto *maybe_value = klass->Methods().at(name); maybe_value != nullptr) {
    return Call(*maybe_value->AsClosure(), arg_count);
  } else {
    RuntimeError(absl::StreamFormat("Undefined property '%s'.", name->Value()));
    return false;
  }
}

bool VM::Invoke(ObjString *name, int arg_count) {
  ObjInstance *receiver = Peek(arg_count).AsInstance();
  if (receiver == nullptr) {
    RuntimeError("Only instances have methods.");
    return false;
  }

  if (auto *maybe_value = receiver->Fields().at(name); maybe_value != nullptr) {
    Value value = stack_top_[-arg_count - 1] = *maybe_value;
    return CallValue(value, arg_count);
  }
  return InvokeFromClass(&receiver->Klass(), name, arg_count);
}

bool VM::BindMethod(ObjKlass &klass, ObjString *name) {
  ObjClosure *method;
  if (auto maybe_value = klass.Methods().at(name); maybe_value != nullptr) {
    method = maybe_value->AsClosure();
  } else {
    RuntimeError(absl::StreamFormat("Undefined property '%s'.", name->Value()));
    return false;
  }
  ObjBoundMethod *bound_method = new ObjBoundMethod(Peek(0), *method);
  Pop();
  Push(bound_method);
  return true;
}

void VM::ResetStack() {
  stack_top_ = stack_;
  frame_count_ = 0;
  open_upvalues_ = nullptr;
}

void VM::DefineNative(std::string_view name, ObjNative::NativeFn function) {
  ObjString *name_str = intern_table_.CreateString(name);
  Push(name_str);
  Push(new ObjNative(std::string(name), function));
  globals_[name_str] = stack_[1];
  Pop();
  Pop();
}

InterpretResult VM::Run() {
  CallFrame *frame = &Frame();
  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    std::cout << "          ";
    for (Value *slot = stack_; slot < stack_top_; ++slot) {
      std::cout << "[" << *slot << "]";
    }
    std::cout << std::endl;
    Frame().closure->Function().GetChunk().DisassembleInstruction(
        Frame().new_ip - Frame().closure->Function().GetChunk().Data());
#endif
    uint8_t instruction = ReadByte();
    switch (static_cast<OpCode>(instruction)) {
    case OpCode::CONSTANT: {
      Value constant = ReadConstant();
      Push(constant);
      break;
    }
    case OpCode::NIL: {
      Push(Nil{});
      break;
    }
    case OpCode::TRUE: {
      Push(true);
      break;
    }
    case OpCode::FALSE: {
      Push(false);
      break;
    }
    case OpCode::POP: {
      Pop();
      break;
    }
    case OpCode::GET_LOCAL: {
      uint8_t slot = ReadByte();
      Push(frame->slots[slot]);
      break;
    }
    case OpCode::SET_LOCAL: {
      uint8_t slot = ReadByte();
      frame->slots[slot] = Peek(0);
      break;
    }
    case OpCode::GET_GLOBAL: {
      const ObjString *name = ReadString();
      auto it = globals_.find(name);
      if (it == globals_.end()) {
        RuntimeError(
            absl::StreamFormat("Undefined variable '%s'.", name->Value()));
        return InterpretResult::RUNTIME_ERROR;
      }
      Push(it->second);
      break;
    }
    case OpCode::DEFINE_GLOBAL: {
      ObjString *name = ReadString();
      globals_[name] = Peek(0);
      Pop();
      break;
    }
    case OpCode::SET_GLOBAL: {
      ObjString *name = ReadString();
      if (globals_.insert_or_assign(name, Peek(0)).second) {
        globals_.erase(name);
        RuntimeError(
            absl::StreamFormat("Undefined variable '%s'.", name->Value()));
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::GET_UPVALUE: {
      uint8_t slot = ReadByte();
      Push(*frame->closure->Upvalues()[slot]->Location());
      break;
    }
    case OpCode::SET_UPVALUE: {
      uint8_t slot = ReadByte();
      *frame->closure->Upvalues()[slot]->Location() = Peek(0);
      break;
    }
    case OpCode::GET_PROPERTY: {
      ObjInstance *instance = Peek(0).AsInstance();
      if (instance == nullptr) {
        RuntimeError("Only instances have properties.");
        return InterpretResult::RUNTIME_ERROR;
      }
      ObjString *name = ReadString();
      if (auto maybe_value = instance->Fields().at(name);
          maybe_value != nullptr) {
        Pop();
        Push(*maybe_value);
        break;
      }

      if (!BindMethod(instance->Klass(), name)) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::SET_PROPERTY: {
      ObjInstance *instance = Peek(1).AsInstance();
      if (instance == nullptr) {
        RuntimeError("Only instances have fields.");
        return InterpretResult::RUNTIME_ERROR;
      }
      instance->Fields()[ReadString()] = Peek(0);
      Value value = Pop();
      Pop();
      Push(value);
      break;
    }
    case OpCode::GET_SUPER: {
      ObjString *name = ReadString();
      ObjKlass *superclass = Pop().AsKlass();
      if (!BindMethod(*superclass, name)) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::EQUAL: {
      Push(Pop() == Pop());
      break;
    }
    case OpCode::GREATER: {
      if (!BinaryOp([](double a, double b) { return a > b; })) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::LESS: {
      if (!BinaryOp([](double a, double b) { return a < b; })) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::ADD: {
      if (Peek(0).IsString() && Peek(1).IsString()) {
        Concatenate();
      } else if (Peek(0).IsNumber() && Peek(1).IsNumber()) {
        auto b = Pop().AsNumber();
        auto a = Pop().AsNumber();
        Push(a + b);
      } else {
        RuntimeError("Operands must be two numbers or two strings.");
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::SUBTRACT: {
      if (!BinaryOp([](double a, double b) { return a - b; })) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::MULTIPLY: {
      if (!BinaryOp([](double a, double b) { return a * b; })) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::DIVIDE: {
      if (!BinaryOp([](double a, double b) { return a / b; })) {
        return InterpretResult::RUNTIME_ERROR;
      }
      break;
    }
    case OpCode::NOT: {
      Push(Pop().IsFalsey());
      break;
    }
    case OpCode::NEGATE: {
      if (!Peek(0).IsNumber()) {
        RuntimeError("Operand must be a number.");
        return InterpretResult::RUNTIME_ERROR;
      }
      Push(-Pop().AsNumber());
      break;
    }
    case OpCode::PRINT: {
      std::cout << Pop() << std::endl;
      break;
    }
    case OpCode::JUMP: {
      uint16_t offset = ReadShort();
      frame->new_ip += offset;
      break;
    }
    case OpCode::JUMP_IF_FALSE: {
      uint16_t offset = ReadShort();
      if (Peek(0).IsFalsey()) {
        frame->new_ip += offset;
      }
      break;
    }
    case OpCode::LOOP: {
      uint16_t offset = ReadShort();
      frame->new_ip -= offset;
      break;
    }
    case OpCode::CALL: {
      int arg_count = ReadByte();
      if (!CallValue(Peek(arg_count), arg_count)) {
        return InterpretResult::RUNTIME_ERROR;
      }
      frame = &Frame();
      break;
    }
    case OpCode::CLOSURE: {
      ObjFunction *function = ReadConstant().AsFunction();
      ObjClosure *closure = new ObjClosure(function);
      Push(closure);
      for (auto &upvalue : closure->Upvalues()) {
        uint8_t is_local = ReadByte();
        uint8_t index = ReadByte();
        if (is_local) {
          upvalue = CaptureUpvalue(frame->slots + index);
        } else {
          upvalue = frame->closure->Upvalues()[index];
        }
      }
      break;
    }
    case OpCode::INVOKE: {
      ObjString *method = ReadString();
      int arg_count = ReadByte();
      if (!Invoke(method, arg_count)) {
        return InterpretResult::RUNTIME_ERROR;
      }
      frame = &Frame();
      break;
    }
    case OpCode::SUPER_INVOKE: {
      ObjString *method = ReadString();
      int arg_count = ReadByte();
      ObjKlass *superclass = Pop().AsKlass();
      if (!InvokeFromClass(superclass, method, arg_count)) {
        return InterpretResult::RUNTIME_ERROR;
      }
      frame = &Frame();
      break;
    }
    case OpCode::CLOSE_UPVALUE: {
      CloseUpvalues(stack_top_ - 1);
      Pop();
      break;
    }
    case OpCode::RETURN: {
      Value result = Pop();
      CloseUpvalues(frame->slots);
      --frame_count_;
      if (frame_count_ == 0) {
        Pop();
        return InterpretResult::OK;
      }

      stack_top_ = frame->slots;
      Push(result);
      frame = &Frame();
      break;
    }
    case OpCode::CLASS: {
      Push(new ObjKlass(*ReadString()));
      break;
    }
    case OpCode::INHERIT: {
      ObjKlass *superclass = Peek(1).AsKlass();
      if (superclass == nullptr) {
        RuntimeError("Superclass must be a class.");
        return InterpretResult::RUNTIME_ERROR;
      }
      ObjKlass *subclass = Peek(0).AsKlass();
      superclass->Methods().for_each([subclass](const auto &value) {
        subclass->Methods()[value.first] = value.second;
      });
      Pop();
      break;
    }
    case OpCode::METHOD: {
      DefineMethod(ReadString());
      break;
    }
    }
  }
}

CallFrame &VM::Frame() { return frames_[frame_count_ - 1]; }

uint8_t VM::ReadByte() {
  uint8_t result = *Frame().new_ip;
  ++Frame().new_ip;
  return result;
}

uint16_t VM::ReadShort() {
  const auto &chunk = Frame().closure->Function().GetChunk();
  Frame().new_ip += 2;
  return ((Frame().new_ip[-2]) << 8) | Frame().new_ip[-1];
}

Value VM::ReadConstant() {
  uint8_t constant_idx = ReadByte();
  return Frame().closure->Function().GetChunk().ReadConstant(constant_idx);
}

ObjString *VM::ReadString() { return ReadConstant().AsString(); }

void VM::Concatenate() {
  const ObjString *b = Peek(0).AsString();
  const ObjString *a = Peek(1).AsString();
  ObjString *result = intern_table_.CreateString(a->Value() + b->Value());
  Pop();
  Pop();
  Push(result);
}

ObjUpvalue *VM::CaptureUpvalue(Value *local) {
  ObjUpvalue **it;
  for (it = &open_upvalues_; *it != nullptr && (*it)->Location() > local;
       it = &(*it)->NextUp())
    ;

  if (*it != nullptr && (*it)->Location() == local) {
    return *it;
  }

  ObjUpvalue *created_upvalue = new ObjUpvalue(local);
  created_upvalue->NextUp() = *it;
  *it = created_upvalue;
  return created_upvalue;
}

void VM::CloseUpvalues(Value *last) {
  while (open_upvalues_ != nullptr && open_upvalues_->Location() >= last) {
    ObjUpvalue *upvalue = open_upvalues_;
    upvalue->Closed() = *upvalue->Location();
    upvalue->Location() = &upvalue->Closed();
    open_upvalues_ = upvalue->NextUp();
  }
}

void VM::DefineMethod(ObjString *name) {
  Value method = Peek(0);
  Peek(1).AsKlass()->Methods()[name] = method;
  Pop();
}
