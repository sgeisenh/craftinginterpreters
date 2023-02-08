#pragma once

#include <memory>
#include <string_view>

#include "common.h"
#include "intern.h"

class Compiler {
public:
  explicit Compiler(InternTable &intern_table, ObjFunction &function);
  ~Compiler();

  bool Compile(std::string_view source);

  std::vector<Obj *> GetRoots();

private:
  class Impl;
  std::unique_ptr<Impl> impl_;
};
