#include "memory.h"

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "value.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <iostream>
#endif

class GarbageCollector::Impl {
public:
  explicit Impl(VM *vm) : vm_(vm) {}

  void CollectGarbage();

  void MarkRoots();
  void TraceReferences();
  void Sweep();

  void MarkObject(Obj *object);
  void MarkValue(Value value);
  void MarkTable(absl::flat_hash_map<ObjString *, Value> &table);
  void MarkFastTable(FastTable &table);
  void MarkArray(absl::Span<const Value> array);
  void BlackenObject(Obj *object);

private:
  VM *vm_ = nullptr;
};

GarbageCollector::GarbageCollector(VM *vm)
    : impl_(std::make_unique<Impl>(vm)) {}

void GarbageCollector::CollectGarbage() { impl_->CollectGarbage(); }

void GarbageCollector::Impl::MarkObject(Obj *object) {
  if (object == nullptr || object->IsMarked()) {
    return;
  }
  object->Mark();
  vm_->gray_stack_.push_back(object);
}

void GarbageCollector::Impl::MarkValue(Value value) {
  Obj *obj = value.AsObject();
  if (obj != nullptr) {
    MarkObject(obj);
  }
}

void GarbageCollector::Impl::MarkTable(
    absl::flat_hash_map<ObjString *, Value> &table) {
  for (const auto &[key, value] : table) {
    MarkObject(key);
    MarkValue(value);
  }
}

void GarbageCollector::Impl::MarkFastTable(FastTable &table) {
  table.for_each([this](const auto &value) {
    MarkObject(value.first);
    MarkValue(value.second);
  });
}

void GarbageCollector::Impl::MarkArray(absl::Span<const Value> array) {
  for (auto value : array) {
    MarkValue(value);
  }
}

void GarbageCollector::Impl::BlackenObject(Obj *object) {
#ifdef DEBUG_LOG_GC
  std::cout << absl::StreamFormat("%p blacken %d: ", object, object->GetType());
  Value(object).Print();
  std::cout << std::endl;
#endif
  switch (object->GetType()) {
  case ObjType::CLOSURE: {
    ObjClosure *closure = object->AsClosure();
    MarkObject(&closure->Function());
    for (auto *upvalue : closure->Upvalues()) {
      MarkObject(upvalue);
    }
    break;
  }
  case ObjType::KLASS: {
    ObjKlass *klass = object->AsKlass();
    MarkObject(&klass->Name());
    MarkFastTable(klass->Methods());
    break;
  }
  case ObjType::INSTANCE: {
    ObjInstance *instance = object->AsInstance();
    MarkObject(&instance->Klass());
    MarkFastTable(instance->Fields());
    break;
  }
  case ObjType::BOUND_METHOD: {
    ObjBoundMethod *bound_method = object->AsBoundMethod();
    MarkValue(bound_method->Receiver());
    MarkObject(&bound_method->Method());
    break;
  }
  case ObjType::FUNCTION: {
    const ObjFunction *function = object->AsFunction();
    MarkObject(function->Name());
    MarkArray(function->GetChunk().Constants());
    break;
  }
  case ObjType::UPVALUE: {
    MarkValue(object->AsUpvalue()->Closed());
    break;
  }
  case ObjType::NATIVE:
  case ObjType::STRING:
    break;
  }
}

void GarbageCollector::Impl::CollectGarbage() {
#ifdef DEBUG_LOG_GC
  std::cout << "-- gc begin\n";
#endif

  MarkRoots();
  TraceReferences();
  vm_->intern_table_.RemoveWhite();
  Sweep();

#ifdef DEBUG_LOG_GC
  std::cout << "-- gc end\n";
#endif
}

void GarbageCollector::Impl::MarkRoots() {
  for (Value *slot = vm_->stack_; slot < vm_->stack_top_; ++slot) {
    MarkValue(*slot);
  }
  for (int i = 0; i < vm_->frame_count_; ++i) {
    MarkObject(vm_->frames_[i].closure);
  }
  for (ObjUpvalue *upvalue = vm_->open_upvalues_; upvalue != nullptr;
       upvalue = upvalue->NextUp()) {
    MarkObject(upvalue);
  }
#ifdef DEBUG_LOG_GC
  std::cout << "-- mark globals begin\n";
#endif
  MarkTable(vm_->globals_);
#ifdef DEBUG_LOG_GC
  std::cout << "-- mark globals end\n";
#endif
  if (vm_->compiler_ != nullptr) {
#ifdef DEBUG_LOG_GC
    std::cout << "-- mark compiler begin\n";
#endif
    for (auto *obj : vm_->compiler_->GetRoots()) {
      MarkObject(obj);
    }
#ifdef DEBUG_LOG_GC
    std::cout << "-- mark compiler end\n";
#endif
  }
  MarkObject(vm_->init_string_);
}

void GarbageCollector::Impl::TraceReferences() {
  while (!vm_->gray_stack_.empty()) {
    Obj *object = vm_->gray_stack_.back();
    vm_->gray_stack_.pop_back();
    BlackenObject(object);
  }
}

void GarbageCollector::Impl::Sweep() {
  Obj *previous = nullptr;
  Obj *object = Obj::Objects();
  while (object != nullptr) {
    if (object->IsMarked()) {
      object->ClearMark();
      previous = object;
      object = previous->Next();
    } else {
      Obj *unreached = object;
      object = object->Next();
      if (previous != nullptr) {
        previous->SetNext(object);
      } else {
        Obj::SetObjects(object);
      }
      delete unreached;
    }
  }
}
