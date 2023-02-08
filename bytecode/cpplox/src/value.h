#pragma once

#include <iostream>
#include <string>
#include <variant>

#include "absl/strings/str_format.h"
#include "common.h"
#include "memory.h"

enum class ObjType {
  STRING,
  CLOSURE,
  FUNCTION,
  NATIVE,
  UPVALUE,
  KLASS,
  INSTANCE,
  BOUND_METHOD,
};

class Obj;
class GarbageCollector;
class ObjString;
class ObjClosure;
class ObjUpvalue;
class ObjKlass;
class ObjInstance;
class ObjBoundMethod;
class ObjFunction;
class ObjNative;

std::ostream &operator<<(std::ostream &, const Obj &obj);

struct Nil {};
bool operator==(Nil a, Nil b);

class Value {
public:
  Value();
  Value(bool value);
  Value(Nil value);
  Value(double value);
  Value(Obj *obj);

  bool IsBool() const;
  bool AsBool() const;

  bool IsNil() const;

  bool IsNumber() const;
  double AsNumber() const;

  bool IsObject() const;
  Obj *AsObject();
  const Obj *AsObject() const;

#define IS_AS(type)                                                            \
  bool Is##type() const;                                                       \
  Obj##type *As##type();                                                       \
  const Obj##type *As##type() const

  IS_AS(String);
  IS_AS(Closure);
  IS_AS(Upvalue);
  IS_AS(Klass);
  IS_AS(Instance);
  IS_AS(BoundMethod);
  IS_AS(Function);
  IS_AS(Native);
#undef IS_AS

  bool IsFalsey() const;
  bool operator==(Value other) const;
  std::ostream &operator<<(std::ostream &os) const;

private:
#ifdef NAN_BOXING
  uint64_t value_;
#else
  std::variant<bool, Nil, double, Obj *> value_;
#endif
};

inline constexpr int kTableSize = 8;

class ArrayTable {
public:
  using key_type = ObjString *;
  using mapped_type = Value;
  using value_type = std::pair<key_type, mapped_type>;
  using size_type = uint8_t;
  using iterator = value_type *;
  using const_iterator = const value_type *;

  uint8_t size() const { return length_; }

  iterator begin() noexcept { return table_.begin(); }
  const_iterator begin() const noexcept { return cbegin(); }
  const_iterator cbegin() const noexcept { return table_.begin(); }

  iterator end() noexcept { return begin() + length_; }
  const_iterator end() const noexcept { return cend(); }
  const_iterator cend() const noexcept { return table_.cbegin() + length_; }

  mapped_type &operator[](ObjString *key) {
    auto it = std::find_if(
        begin(), end(), [key](const auto &elem) { return key == elem.first; });
    if (it != end()) {
      return it->second;
    } else {
      table_[length_].first = key;
      return table_[length_++].second;
    }
  }

  iterator find(key_type key) {
    return std::find_if(begin(), end(),
                        [key](const auto &elem) { return key == elem.first; });
  }

private:
  std::array<value_type, 8> table_;
  uint8_t length_ = 0;
};

class FastTable {
public:
  using key_type = ObjString *;
  using mapped_type = Value;
  using value_type = std::pair<key_type, mapped_type>;
  using size_type = uint8_t;
  using iterator = value_type *;
  using const_iterator = const iterator;

  FastTable() : table_(ArrayTable()) {}

  mapped_type *at(key_type key) {
    return std::visit(
        [key](auto &arg) -> mapped_type * {
          if (auto it = arg.find(key); it != arg.end()) {
            return &it->second;
          } else {
            return nullptr;
          }
        },
        table_);
  }

  mapped_type &operator[](ObjString *key) {
    if (auto *table = std::get_if<ArrayTable>(&table_); table != nullptr) {
      if (table->size() < 8) {
        return (*table)[key];
      }

      table_ =
          absl::flat_hash_map<ObjString *, Value>(table->begin(), table->end());
    }
    return std::get<absl::flat_hash_map<ObjString *, Value>>(table_)[key];
  }

  template <typename F> void for_each(F callback) {
    return std::visit(
        [&](auto &arg) {
          for (auto &elem : arg) {
            callback(elem);
          }
        },
        table_);
  }

private:
  std::variant<ArrayTable, absl::flat_hash_map<ObjString *, Value>> table_;
};

enum class OpCode : uint8_t {
  CONSTANT,
  NIL,
  TRUE,
  FALSE,
  POP,
  GET_LOCAL,
  SET_LOCAL,
  GET_GLOBAL,
  DEFINE_GLOBAL,
  SET_GLOBAL,
  GET_UPVALUE,
  SET_UPVALUE,
  GET_PROPERTY,
  SET_PROPERTY,
  GET_SUPER,
  EQUAL,
  GREATER,
  LESS,
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
  NOT,
  NEGATE,
  PRINT,
  JUMP,
  JUMP_IF_FALSE,
  LOOP,
  CALL,
  INVOKE,
  SUPER_INVOKE,
  CLOSURE,
  CLOSE_UPVALUE,
  RETURN,
  CLASS,
  INHERIT,
  METHOD,
};

class Chunk {
public:
  void Write(uint8_t byte, int line) {
    code_.push_back(byte);
    lines_.push_back(line);
  }

  int AddConstant(Value value) {
    constants_.push_back(value);
    return constants_.size() - 1;
  }
  uint8_t &operator[](int offset) { return code_[offset]; }
  uint8_t operator[](int offset) const { return code_[offset]; }

  const uint8_t *Data() const { return code_.data(); }
  int Size() const { return code_.size(); }
  Value ReadConstant(int offset) const { return constants_[offset]; }
  const std::vector<Value> &Constants() const { return constants_; }
  int GetLine(int offset) const { return lines_[offset]; }

  void Disassemble(std::string_view name) const;
  int DisassembleInstruction(int offset) const;

private:
  int ConstantInstruction(std::string_view name, int offset) const;
  int ByteInstruction(std::string_view name, int offset) const;
  int JumpInstruction(std::string_view name, int sign, int offset) const;
  int InvokeInstruction(std::string_view name, int offset) const;

  std::vector<uint8_t> code_;
  std::vector<int> lines_;
  std::vector<Value> constants_;
};

class Obj {
public:
  virtual ~Obj();

  ObjType GetType() const;

  virtual std::ostream &operator<<(std::ostream &os) const = 0;

  bool operator==(const Obj &other) const;

#define AS(type)                                                               \
  Obj##type *As##type();                                                       \
  const Obj##type *As##type() const;

  AS(String);
  AS(Closure);
  AS(Upvalue);
  AS(Klass);
  AS(Instance);
  AS(BoundMethod);
  AS(Function);
  AS(Native);
#undef AS

  void Mark() const;

  bool IsMarked();
  void ClearMark();
  Obj *Next();
  void SetNext(Obj *next);

  static Obj *Objects();
  static void SetObjects(Obj *objects);
  static void FreeObjects();
  static void SetGc(GarbageCollector *gc);

protected:
  explicit Obj(ObjType type);
  void Bookkeeping();

private:
  static GarbageCollector *gc_;
  static Obj *objects_;

  ObjType type_;
  mutable bool is_marked_ = false;
  Obj *next_;
};

class ObjString : public Obj {
public:
  explicit ObjString(std::string value);

  std::ostream &operator<<(std::ostream &os) const override;

  const std::string &Value() const;

private:
  std::string contents_;
};

class ObjUpvalue : public Obj {
public:
  explicit ObjUpvalue(Value *slot);
  Value *&Location();
  ObjUpvalue *&NextUp();
  Value &Closed();

  std::ostream &operator<<(std::ostream &os) const override;

private:
  Value *location_;
  ObjUpvalue *next_ = nullptr;
  Value closed_ = Nil();
};

class ObjKlass : public Obj {
public:
  explicit ObjKlass(ObjString &name);

  ObjString &Name();
  const ObjString &Name() const;

  FastTable &Methods();

  std::ostream &operator<<(std::ostream &os) const override;

private:
  ObjString *name_;
  FastTable methods_;
};

class ObjInstance : public Obj {
public:
  explicit ObjInstance(ObjKlass &klass);

  ObjKlass &Klass();
  FastTable &Fields();

  std::ostream &operator<<(std::ostream &os) const override;

private:
  ObjKlass *klass_;
  FastTable fields_;
};

class ObjBoundMethod : public Obj {
public:
  explicit ObjBoundMethod(Value receiver, ObjClosure &method);

  Value Receiver();
  ObjClosure &Method();

  std::ostream &operator<<(std::ostream &os) const override;

private:
  Value receiver_;
  ObjClosure *method_;
};

class ObjFunction : public Obj {
public:
  explicit ObjFunction();
  explicit ObjFunction(int arity, ObjString *name);

  int Arity() const;
  void SetArity(int arity);

  Chunk &GetChunk();
  const Chunk &GetChunk() const;

  ObjString *Name() const;
  void SetName(ObjString &name);

  int UpvalueCount() const;

  void IncrementUpvalueCount();

  std::ostream &operator<<(std::ostream &os) const override;

private:
  int arity_;
  Chunk chunk_;
  ObjString *name_;
  int upvalue_count_ = 0;
};

class ObjClosure : public Obj {
public:
  explicit ObjClosure(ObjFunction *function);

  ObjFunction &Function();
  std::vector<ObjUpvalue *> &Upvalues();
  std::ostream &operator<<(std::ostream &os) const override;

private:
  ObjFunction *function_;
  std::vector<ObjUpvalue *> upvalues_;
};

struct ObjNative : public Obj {
  typedef Value (*NativeFn)(int arg_count, Value *args);

  ObjNative(std::string name, NativeFn function);

  std::string_view Name() const;

  Value Call(int arg_count, Value *args) const;

  std::ostream &operator<<(std::ostream &os) const override;

private:
  std::string name_;
  NativeFn function_;
};

struct InternHash {
  using is_transparent = void;

  size_t operator()(const ObjString *value) const {
    return std::hash<std::string>()(value->Value());
  }

  size_t operator()(std::string_view value) const {
    return std::hash<std::string_view>()(value);
  }
};

struct InternEqual {
  using is_transparent = void;

  bool operator()(const ObjString *a, const ObjString *b) const {
    return a->Value() == b->Value();
  }

  bool operator()(const ObjString *a, std::string_view b) const {
    return a->Value() == b;
  }

  bool operator()(std::string_view a, const ObjString *b) const {
    return a == b->Value();
  }
};

// Obj implementation

inline Obj::~Obj() = default;
inline ObjType Obj::GetType() const { return type_; }
inline bool Obj::operator==(const Obj &other) const {
  // Pointer equality :D
  return this == &other;
}

#define AS(type, caps)                                                         \
  inline Obj##type *Obj::As##type() {                                          \
    if (type_ != ObjType::caps) {                                              \
      return nullptr;                                                          \
    }                                                                          \
    return static_cast<Obj##type *>(this);                                     \
  }                                                                            \
  inline const Obj##type *Obj::As##type() const {                              \
    if (type_ != ObjType::caps) {                                              \
      return nullptr;                                                          \
    }                                                                          \
    return static_cast<const Obj##type *>(this);                               \
  }
AS(String, STRING)
AS(Closure, CLOSURE)
AS(Upvalue, UPVALUE)
AS(Function, FUNCTION)
AS(Klass, KLASS)
AS(Instance, INSTANCE)
AS(BoundMethod, BOUND_METHOD)
AS(Native, NATIVE)
#undef AS

inline void Obj::Mark() const {
  is_marked_ = true;
#ifdef DEBUG_LOG_GC
  std::cout << absl::StreamFormat("%p mark ", this);
  this->operator<<(std::cout);
  std::cout << std::endl;
#endif
}
inline bool Obj::IsMarked() { return is_marked_; }
inline void Obj::ClearMark() { is_marked_ = false; }
inline Obj *Obj::Next() { return next_; }
inline void Obj::SetNext(Obj *next) { next_ = next; }
inline Obj *Obj::Objects() { return objects_; }
inline void Obj::SetObjects(Obj *objects) { objects_ = objects; }
inline void Obj::FreeObjects() {
  Obj *object = objects_;
  while (object != nullptr) {
    Obj *next = object->next_;
    delete object;
    object = next;
  }
}
inline void Obj::SetGc(GarbageCollector *gc) { gc_ = gc; }
inline Obj::Obj(ObjType type) : type_(type) {}
inline Obj *Obj::objects_ = nullptr;
inline GarbageCollector *Obj::gc_ = nullptr;
inline std::ostream &operator<<(std::ostream &os, const Obj &obj) {
  return obj.operator<<(os);
}

// ObjString implementation.

inline ObjString::ObjString(std::string value)
    : Obj(ObjType::STRING), contents_(std::move(value)) {
  Bookkeeping();
}
inline std::ostream &ObjString::operator<<(std::ostream &os) const {
  os << Value();
  return os;
}
inline const std::string &ObjString::Value() const { return contents_; }

// ObjUpvalue implementation.

inline ObjUpvalue::ObjUpvalue(Value *slot)
    : Obj(ObjType::UPVALUE), location_(slot) {
  Bookkeeping();
}
inline Value *&ObjUpvalue::Location() { return location_; }
inline ObjUpvalue *&ObjUpvalue::NextUp() { return next_; }
inline Value &ObjUpvalue::Closed() { return closed_; }
inline std::ostream &ObjUpvalue::operator<<(std::ostream &os) const {
  return os << "upvalue";
}

// ObjKlass implementation.

inline ObjKlass::ObjKlass(ObjString &name) : Obj(ObjType::KLASS), name_(&name) {
  Bookkeeping();
}
inline ObjString &ObjKlass::Name() { return *name_; }
inline const ObjString &ObjKlass::Name() const { return *name_; }
inline FastTable &ObjKlass::Methods() { return methods_; }
inline std::ostream &ObjKlass::operator<<(std::ostream &os) const {
  return os << *name_;
}

// ObjInstance implementation.

inline ObjInstance::ObjInstance(ObjKlass &klass)
    : Obj(ObjType::INSTANCE), klass_(&klass) {
  Bookkeeping();
}
inline ObjKlass &ObjInstance::Klass() { return *klass_; }
inline FastTable &ObjInstance::Fields() { return fields_; }
inline std::ostream &ObjInstance::operator<<(std::ostream &os) const {
  return os << *klass_ << " instance";
}

// ObjBoundMethod implementation.

inline ObjBoundMethod::ObjBoundMethod(Value receiver, ObjClosure &method)
    : Obj(ObjType::BOUND_METHOD), receiver_(receiver), method_(&method) {}
inline Value ObjBoundMethod::Receiver() { return receiver_; }
inline ObjClosure &ObjBoundMethod::Method() { return *method_; }
inline std::ostream &ObjBoundMethod::operator<<(std::ostream &os) const {
  return os << *method_;
}

// ObjFunction implementation.

inline ObjFunction::ObjFunction() : ObjFunction(0, nullptr) {}
inline ObjFunction::ObjFunction(int arity, ObjString *name)
    : Obj(ObjType::FUNCTION), arity_(arity), name_(name) {
  Bookkeeping();
}
inline int ObjFunction::Arity() const { return arity_; }
inline void ObjFunction::SetArity(int arity) { arity_ = arity; }
inline Chunk &ObjFunction::GetChunk() { return chunk_; }
inline const Chunk &ObjFunction::GetChunk() const { return chunk_; }
inline ObjString *ObjFunction::Name() const { return name_; }
inline void ObjFunction::SetName(ObjString &name) { name_ = &name; }
inline int ObjFunction::UpvalueCount() const { return upvalue_count_; }
inline void ObjFunction::IncrementUpvalueCount() { ++upvalue_count_; }
inline std::ostream &ObjFunction::operator<<(std::ostream &os) const {
  if (name_ == nullptr) {
    return os << "<script>";
  } else {
    return os << "<fn " << *name_ << ">";
  }
}

// ObjClosure implementation.

inline ObjClosure::ObjClosure(ObjFunction *function)
    : Obj(ObjType::CLOSURE), function_(function),
      upvalues_(std::vector<ObjUpvalue *>(function->UpvalueCount(), nullptr)) {
  Bookkeeping();
}
inline ObjFunction &ObjClosure::Function() { return *function_; }
inline std::vector<ObjUpvalue *> &ObjClosure::Upvalues() { return upvalues_; }
inline std::ostream &ObjClosure::operator<<(std::ostream &os) const {
  return os << *function_;
}

// ObjNative implementation.

inline ObjNative::ObjNative(std::string name, NativeFn function)
    : Obj(ObjType::NATIVE), name_(std::move(name)), function_(function) {
  Bookkeeping();
}
inline std::string_view ObjNative::Name() const { return name_; }
inline Value ObjNative::Call(int arg_count, Value *args) const {
  return function_(arg_count, args);
}
inline std::ostream &ObjNative::operator<<(std::ostream &os) const {
  // TODO: Add name back in when not running tests.
  return os << "<native fn>";
}

inline bool operator==(Nil a, Nil b) { return true; }

// Value implementation.

inline Value::Value() : Value(Nil()) {}
inline bool Value::IsFalsey() const {
  return IsNil() || (IsBool() && !AsBool());
}
#define IS_AS(type, caps)                                                      \
  inline bool Value::Is##type() const {                                        \
    return IsObject() && AsObject()->GetType() == ObjType::caps;               \
  }                                                                            \
  inline Obj##type *Value::As##type() {                                        \
    auto maybe_obj = AsObject();                                               \
    return maybe_obj == nullptr ? nullptr : maybe_obj->As##type();             \
  }                                                                            \
  inline const Obj##type *Value::As##type() const {                            \
    auto maybe_obj = AsObject();                                               \
    return maybe_obj == nullptr ? nullptr : maybe_obj->As##type();             \
  }

IS_AS(String, STRING)
IS_AS(Closure, CLOSURE)
IS_AS(Upvalue, UPVALUE)
IS_AS(Klass, KLASS)
IS_AS(Instance, INSTANCE)
IS_AS(BoundMethod, BOUND_METHOD)
IS_AS(Function, FUNCTION)
IS_AS(Native, NATIVE)
#undef IS_AS
inline std::ostream &operator<<(std::ostream &os, Value value) {
  return value.operator<<(os);
}

#ifdef NAN_BOXING
inline constexpr uint64_t kQnan = 0x7ffc000000000000ull;
inline constexpr uint64_t kTagNil = 0b01ull;
inline constexpr uint64_t kTagFalse = 0b10ull;
inline constexpr uint64_t kTagTrue = 0b11ull;
inline constexpr uint64_t kNilVal = kQnan | kTagNil;
inline constexpr uint64_t kFalseVal = kQnan | kTagFalse;
inline constexpr uint64_t kTrueVal = kQnan | kTagTrue;
inline constexpr uint64_t kSignBit = 1ull << 63ull;

inline Value::Value(bool value) : value_(value ? kTrueVal : kFalseVal) {}
inline Value::Value(Nil value) : value_(kNilVal) {}
inline Value::Value(double value)
    : value_(*reinterpret_cast<uint64_t *>(&value)) {}
inline Value::Value(Obj *obj)
    : value_(kSignBit | kQnan | reinterpret_cast<uint64_t>(obj)) {}
inline bool Value::IsBool() const { return (value_ | 1) == kTrueVal; }
inline bool Value::AsBool() const { return value_ == kTrueVal; }
inline bool Value::IsNil() const { return value_ == kNilVal; }
inline bool Value::IsNumber() const { return (value_ & kQnan) != kQnan; }
inline double Value::AsNumber() const {
  return *reinterpret_cast<const double *>(&value_);
}
inline bool Value::IsObject() const {
  return (value_ & (kQnan | kSignBit)) == (kQnan | kSignBit);
}
inline Obj *Value::AsObject() {
  if (!IsObject()) {
    return nullptr;
  }
  return reinterpret_cast<Obj *>(value_ & ~(kSignBit | kQnan));
}
inline const Obj *Value::AsObject() const {
  if (!IsObject()) {
    return nullptr;
  }
  return reinterpret_cast<const Obj *>(value_ & ~(kSignBit | kQnan));
}
inline bool Value::operator==(Value other) const {
  if (IsNumber() && other.IsNumber()) {
    return AsNumber() == other.AsNumber();
  }
  return value_ == other.value_;
}

inline std::ostream &Value::operator<<(std::ostream &os) const {
  if (IsBool()) {
    return os << (AsBool() ? "true" : "false");
  } else if (IsNil()) {
    return os << "nil";
  } else if (IsNumber()) {
    return os << AsNumber();
  } else if (IsObject()) {
    return os << *AsObject();
  } else {
    std::cerr << "Invalid value representation" << std::endl;
    exit(1);
  }
}
#else
inline Value::Value(bool value) : value_(value) {}
inline Value::Value(Nil value) : value_(value) {}
inline Value::Value(double value) : value_(value) {}
inline Value::Value(Obj *obj) : value_(obj) {}
inline bool Value::IsBool() const {
  return std::holds_alternative<bool>(value_);
}
inline bool Value::AsBool() const { return std::get<bool>(value_); }
inline bool Value::IsNil() const { return std::holds_alternative<Nil>(value_); }
inline bool Value::IsNumber() const {
  return std::holds_alternative<double>(value_);
}
inline double Value::AsNumber() const { return std::get<double>(value_); }
inline bool Value::IsObject() const {
  return std::holds_alternative<Obj *>(value_);
}
inline Obj *Value::AsObject() {
  auto maybe_obj = std::get_if<Obj *>(&value_);
  return maybe_obj == nullptr ? nullptr : *maybe_obj;
}
inline const Obj *Value::AsObject() const {
  auto maybe_obj = std::get_if<Obj *>(&value_);
  return maybe_obj == nullptr ? nullptr : *maybe_obj;
}
inline bool Value::operator==(Value other) const {
  return value_ == other.value_;
}

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

inline std::ostream &Value::operator<<(std::ostream &os) const {
  return std::visit(overloaded{
                        [&](bool arg) -> std::ostream & {
                          return os << (arg ? "true" : "false");
                        },
                        [&](double arg) -> std::ostream & { return os << arg; },
                        [&](Nil arg) -> std::ostream & { return os << "nil"; },
                        [&](Obj *arg) -> std::ostream & { return os << *arg; },
                    },
                    value_);
}
#endif
