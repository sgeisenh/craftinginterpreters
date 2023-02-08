#include "intern.h"

#include <iostream>
#include <string_view>

#include "absl/container/flat_hash_set.h"

ObjString *InternTable::CreateString(std::string_view value) {
  if (auto it = table_.find(value); it != table_.end()) {
    return *it;
  }
  auto *result = new ObjString(std::string(value));
  table_.insert(result);
  return result;
}

ObjString *InternTable::CreateString(std::string value) {
  if (auto it = table_.find(value); it != table_.end()) {
    return *it;
  }
  auto *result = new ObjString(std::move(value));
  table_.insert(result);
  return result;
}

void InternTable::RemoveWhite() {
  absl::erase_if(table_, [](ObjString *value) {
    return value != nullptr && !value->IsMarked();
  });
}
