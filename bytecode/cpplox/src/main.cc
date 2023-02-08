#include <cstdint>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>

#include "vm.h"

namespace {

template <typename T> uint8_t cnv(T value) {
  return static_cast<uint8_t>(value);
}

void Repl(VM &vm) {
  std::string line;
  for (;;) {
    std::cout << "> ";
    if (!std::getline(std::cin, line)) {
      return;
    }
    vm.Interpret(line);
  }
}

std::string ReadFile(std::string_view path) {
  std::ifstream in(path.data(), std::ios::in | std::ios::binary);
  if (in) {
    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return (contents);
  }
  throw(errno);
}

void RunFile(VM &vm, std::string_view path) {
  std::string source = ReadFile(path);
  InterpretResult result = vm.Interpret(source);
  if (result == InterpretResult::COMPILE_ERROR) {
    exit(65);
  }
  if (result == InterpretResult::RUNTIME_ERROR) {
    exit(70);
  }
}

} // namespace

int main(int argc, char **argv) {
  VM vm;

  if (argc == 1) {
    Repl(vm);
  } else if (argc == 2) {
    RunFile(vm, argv[1]);
  } else {
    std::cout << "Usage: clox [path]" << std::endl;
    exit(64);
  }
  return 0;
}
