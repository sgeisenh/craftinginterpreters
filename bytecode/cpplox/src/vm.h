#pragma once

#include <cstdint>
#include <iostream>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_format.h"
#include "common.h"
#include "compiler.h"
#include "intern.h"
#include "value.h"

enum class InterpretResult {
  OK,
  COMPILE_ERROR,
  RUNTIME_ERROR,
};

// FIXME: This should not be public.
struct CallFrame {
  ObjClosure *closure;
  const uint8_t *new_ip;
  Value *slots;
};

class VM {
public:
  VM();
  ~VM();
  InterpretResult Interpret(std::string_view source);
  void Push(Value value);
  Value Pop();

  void ResetStack();

private:
  friend GarbageCollector;

  template <typename ErrorMessage>
  void RuntimeError(const ErrorMessage &error_message) {
    std::cerr << error_message << "\n";
    for (int i = frame_count_ - 1; i >= 0; --i) {
      const CallFrame &frame = frames_[i];
      const auto &function = frame.closure->Function();
      const uint8_t *instruction = frame.new_ip;
      std::cerr << absl::StreamFormat(
          "[line %d] in ", function.GetChunk().GetLine(
                               instruction - function.GetChunk().Data()));
      if (function.Name() == nullptr) {
        std::cerr << "script\n";
      } else {
        std::cerr << function.Name()->Value() << "()\n";
      }
    }
    ResetStack();
  }
  void DefineNative(std::string_view name, ObjNative::NativeFn function);
  Value Peek(int distance);
  bool Call(ObjClosure &closure, int arg_count);
  bool CallValue(Value callee, int arg_count);
  bool InvokeFromClass(ObjKlass *klass, ObjString *name, int arg_count);
  bool Invoke(ObjString *name, int arg_count);
  bool BindMethod(ObjKlass &klass, ObjString *name);
  InterpretResult Run();
  CallFrame &Frame();
  uint8_t ReadByte();
  uint16_t ReadShort();
  Value ReadConstant();
  ObjString *ReadString();
  void Concatenate();
  ObjUpvalue *CaptureUpvalue(Value *local);
  void CloseUpvalues(Value *last);
  void DefineMethod(ObjString *name);

  const ObjString *Intern(std::string_view value) const;
  void InsertString(const ObjString *value);

  template <typename Op> bool BinaryOp(Op op) {
    if (!Peek(0).IsNumber() || !Peek(1).IsNumber()) {
      RuntimeError("Operands must be numbers.");
      return false;
    }
    auto b = Pop().AsNumber();
    auto a = Pop().AsNumber();
    Push(op(a, b));
    return true;
  }

  CallFrame frames_[kFramesMax] = {};
  int frame_count_ = 0;
  Value stack_[kStackMax] = {};
  Value *stack_top_ = stack_;
  InternTable intern_table_;
  ObjString *init_string_ = nullptr;
  ObjUpvalue *open_upvalues_ = nullptr;
  absl::flat_hash_map<ObjString *, Value> globals_;
  Compiler *compiler_;
  std::vector<Obj *> gray_stack_;
};
