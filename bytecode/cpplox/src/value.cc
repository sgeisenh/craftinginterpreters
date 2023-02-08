#include "value.h"

#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <string_view>
#include <vector>

#include "absl/strings/str_format.h"

namespace {

std::size_t bytes_allocated = 0;
std::size_t next_gc = 1024 * 1024;

constexpr int kGrowFactor = 2;

int SimpleInstruction(std::string_view name, int offset) {
  std::cout << name << "\n";
  return offset + 1;
}

} // namespace

void Chunk::Disassemble(std::string_view name) const {
  std::cout << "== " << name << " ==\n";
  for (int offset = 0; offset < code_.size();) {
    offset = DisassembleInstruction(offset);
  }
}

int Chunk::DisassembleInstruction(int offset) const {
  std::cout << absl::StreamFormat("%04d ", offset);
  if (offset > 0 && lines_[offset] == lines_[offset - 1]) {
    std::cout << "   | ";
  } else {
    std::cout << absl::StreamFormat("%4d ", lines_[offset]);
  }
  switch (static_cast<OpCode>(code_[offset])) {
  case OpCode::CONSTANT: {
    return ConstantInstruction("OP_CONSTANT", offset);
  }
  case OpCode::NIL: {
    return SimpleInstruction("OP_NIL", offset);
  }
  case OpCode::TRUE: {
    return SimpleInstruction("OP_TRUE", offset);
  }
  case OpCode::FALSE: {
    return SimpleInstruction("OP_FALSE", offset);
  }
  case OpCode::POP: {
    return SimpleInstruction("OP_POP", offset);
  }
  case OpCode::GET_LOCAL: {
    return ByteInstruction("OP_GET_LOCAL", offset);
  }
  case OpCode::SET_LOCAL: {
    return ByteInstruction("OP_SET_LOCAL", offset);
  }
  case OpCode::GET_GLOBAL: {
    return ConstantInstruction("OP_GET_GLOBAL", offset);
  }
  case OpCode::DEFINE_GLOBAL: {
    return ConstantInstruction("OP_DEFINE_GLOBAL", offset);
  }
  case OpCode::SET_GLOBAL: {
    return ConstantInstruction("OP_SET_GLOBAL", offset);
  }
  case OpCode::GET_UPVALUE: {
    return ByteInstruction("OP_GET_UPVALUE", offset);
  }
  case OpCode::SET_UPVALUE: {
    return ByteInstruction("OP_SET_UPVALUE", offset);
  }
  case OpCode::GET_PROPERTY: {
    return ConstantInstruction("OP_GET_PROPERTY", offset);
  }
  case OpCode::SET_PROPERTY: {
    return ConstantInstruction("OP_SET_PROPERTY", offset);
  }
  case OpCode::GET_SUPER: {
    return ConstantInstruction("OP_GET_SUPER", offset);
  }
  case OpCode::EQUAL: {
    return SimpleInstruction("OP_EQUAL", offset);
  }
  case OpCode::GREATER: {
    return SimpleInstruction("OP_GREATER", offset);
  }
  case OpCode::LESS: {
    return SimpleInstruction("OP_LESS", offset);
  }
  case OpCode::ADD: {
    return SimpleInstruction("OP_ADD", offset);
  }
  case OpCode::SUBTRACT: {
    return SimpleInstruction("OP_SUBTRACT", offset);
  }
  case OpCode::MULTIPLY: {
    return SimpleInstruction("OP_MULTIPLY", offset);
  }
  case OpCode::DIVIDE: {
    return SimpleInstruction("OP_DIVIDE", offset);
  }
  case OpCode::NEGATE: {
    return SimpleInstruction("OP_NEGATE", offset);
  }
  case OpCode::NOT: {
    return SimpleInstruction("OP_NOT", offset);
  }
  case OpCode::PRINT: {
    return SimpleInstruction("OP_PRINT", offset);
  }
  case OpCode::JUMP: {
    return JumpInstruction("OP_JUMP", 1, offset);
  }
  case OpCode::JUMP_IF_FALSE: {
    return JumpInstruction("OP_JUMP_IF_FALSE", 1, offset);
  }
  case OpCode::LOOP: {
    return JumpInstruction("OP_LOOP", -1, offset);
  }
  case OpCode::CALL: {
    return ByteInstruction("OP_CALL", offset);
  }
  case OpCode::INVOKE: {
    return InvokeInstruction("OP_INVOKE", offset);
  }
  case OpCode::SUPER_INVOKE: {
    return InvokeInstruction("OP_SUPER_INVOKE", offset);
  }
  case OpCode::CLOSURE: {
    uint8_t constant = code_[offset + 1];
    std::cout << absl::StreamFormat("%-16s %4d ", "OP_CLOSURE", constant)
              << constants_[constant] << "\n";

    const ObjFunction *function = constants_[constant].AsFunction();
    for (int j = 0; j < function->UpvalueCount(); ++j) {
      int is_local = code_[offset];
      int index = code_[offset + 1];
      offset += 2;
      std::cout << absl::StreamFormat("%04d      |                     %s %d\n",
                                      offset - 2,
                                      is_local ? "local" : "upvalue", index);
    }
    return offset + 2;
  }
  case OpCode::CLOSE_UPVALUE: {
    return SimpleInstruction("OP_CLOSE_UPVALUE", offset);
  }
  case OpCode::RETURN: {
    return SimpleInstruction("OP_RETURN", offset);
  }
  case OpCode::CLASS: {
    return ConstantInstruction("OP_CLASS", offset);
  }
  case OpCode::INHERIT: {
    return SimpleInstruction("OP_INHERIT", offset);
  }
  case OpCode::METHOD: {
    return ConstantInstruction("OP_METHOD", offset);
  }
  default: {
    std::cout << "Unknown opcode " << code_[offset] << "\n";
    return offset + 1;
  }
  }
}

int Chunk::ConstantInstruction(std::string_view name, int offset) const {
  uint8_t constant = code_[offset + 1];
  std::cout << absl::StreamFormat("%-16s %4d '", name, constant)
            << constants_[constant] << "'\n";
  return offset + 2;
}

int Chunk::ByteInstruction(std::string_view name, int offset) const {
  uint8_t slot = code_[offset + 1];
  std::cout << absl::StreamFormat("%-16s %4d\n", name, slot);
  return offset + 2;
}

int Chunk::JumpInstruction(std::string_view name, int sign, int offset) const {
  uint16_t jump = (code_[offset + 1] << 8) | code_[offset + 2];
  std::cout << absl::StreamFormat("%-16s %4d -> %d\n", name, offset,
                                  offset + 3 + sign * jump);
  return offset + 3;
}

int Chunk::InvokeInstruction(std::string_view name, int offset) const {
  uint8_t constant = code_[offset + 1];
  uint8_t arg_count = code_[offset + 2];
  std::cout << absl::StreamFormat("%-16s (%d args) %4d '", name, arg_count,
                                  constant)
            << constants_[constant] << "'" << std::endl;
  return offset + 3;
}

void *operator new(std::size_t sz) {
  sz += sizeof(std::size_t);
  bytes_allocated += sz;
  if (std::size_t *ptr = static_cast<size_t *>(std::malloc(sz));
      ptr != nullptr) {
    *ptr = sz;
    return ptr + 1;
  }

  throw std::bad_alloc{};
}

void operator delete(void *ptr) noexcept {
  std::size_t *actual = static_cast<std::size_t *>(ptr) - 1;
  bytes_allocated -= *actual;
  std::free(actual);
}

void Obj::Bookkeeping() {
#ifdef DEBUG_LOG_GC
  std::cout << absl::StreamFormat("%p allocate for %d: ", this, type_);
  this->operator<<(std::cout);
  std::cout << std::endl;
#endif
  // We potentially trigger GC whenever we allocate a new object.
#ifdef DEBUG_STRESS_GC
  gc_->CollectGarbage();
  next_gc = bytes_allocated * kGrowFactor;
#endif
  if (bytes_allocated > next_gc) {
    gc_->CollectGarbage();
    next_gc = bytes_allocated * kGrowFactor;
  }

  next_ = objects_;
  objects_ = this;
}
