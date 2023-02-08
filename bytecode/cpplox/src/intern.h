#pragma once

#include <string_view>

#include "absl/container/flat_hash_set.h"
#include "common.h"
#include "value.h"

class InternTable {
public:
  ObjString *CreateString(std::string value);
  ObjString *CreateString(std::string_view value);
  void RemoveWhite();

private:
  absl::flat_hash_set<ObjString *, InternHash, InternEqual> table_;
};
