#pragma once

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "common.h"
#include "value.h"

class VM;

class GarbageCollector {
public:
  explicit GarbageCollector(VM *vm);
  void CollectGarbage();

private:
  class Impl;
  std::unique_ptr<Impl> impl_;
};
