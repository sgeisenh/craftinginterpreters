#include "compiler.h"

#include <charconv>
#include <functional>
#include <iomanip>
#include <iostream>
#include <optional>
#include <string_view>

#include "absl/container/flat_hash_map.h"
#include "absl/strings/str_format.h"
#include "intern.h"
#include "scanner.h"
#include "vm.h"

namespace {

constexpr int kUint8tCount = std::numeric_limits<uint8_t>::max() + 1;

enum class Precedence {
  NONE,
  ASSIGNMENT, // =
  OR,         // or
  AND,        // and
  EQUALITY,   // == !=
  COMPARISON, // < > <= >=
  TERM,       // + -
  FACTOR,     // * /
  UNARY,      // ! -
  CALL,       // . ()
  PRIMARY
};

struct Local {
  Token name;
  int depth;
  bool is_captured;
};

struct Upvalue {
  uint8_t index;
  bool is_local;
};

struct ClassCompiler {
  ClassCompiler *enclosing = nullptr;
  bool has_superclass = false;
};

Token SyntheticToken(std::string_view lexeme) {
  return Token{
      .type = TokenType::IDENTIFIER,
      .lexeme = lexeme,
      .line = -1,
  };
}

} // namespace

class Compiler::Impl {
public:
  enum class FunctionType {
    FUNCTION,
    INITIALIZER,
    METHOD,
    SCRIPT,
  };

  explicit Impl(InternTable &intern_table, ObjFunction &function,
                FunctionType type);

  ~Impl() { current_compiler_ = enclosing_; }

  bool Compile(std::string_view source);

  std::vector<Obj *> GetRoots();

private:
  using RuleFn = std::function<void(Compiler::Impl *, bool)>;
  struct ParseRule {
    RuleFn prefix;
    RuleFn infix;
    Precedence precedence;
  };

  // This could be compile-time constant if it turns out to be important for
  // performance purposes.
  //
  // Compilation tends to not be on the hot-path for small programs.
  //
  // ¯\_(ツ)_/¯
  static absl::flat_hash_map<TokenType, ParseRule> rules_;

  void Advance();
  void Consume(TokenType type, std::string_view message);
  template <typename T> void EmitByte(T value) {
    CurrentChunk().Write(static_cast<uint8_t>(value), previous_.line);
  }
  template <typename T, typename U> void EmitBytes(T left, U right) {
    EmitByte(left);
    EmitByte(right);
  }
  int EmitJump(OpCode instruction);
  void EmitLoop(int loop_start);

  bool Match(TokenType type);
  bool Check(TokenType type);
  void ErrorAtCurrent(std::string_view message);
  void Error(std::string_view message);
  void ErrorAt(const Token &token, std::string_view message);
  void End();
  void BeginScope();
  void EndScope();
  void EmitReturn();
  void Declaration();
  void VarDeclaration();
  void ClassDeclaration();
  void FunDeclaration();
  void Statement();
  void PrintStatement();
  void ReturnStatement();
  void WhileStatement();
  void ExpressionStatement();
  void ForStatement();
  void IfStatement();
  void Synchronize();
  void Expression();
  void Block();
  void Function(FunctionType type);
  void Method();
  void Number(bool can_assign);
  void String(bool can_assign);
  void NamedVariable(const Token &name, bool can_assign);
  void Variable(bool can_assign);
  void This(bool can_assign);
  void Super(bool can_assign);
  void Grouping(bool can_assign);
  void Unary(bool can_assign);
  void Binary(bool can_assign);
  void Call(bool can_assign);
  void Dot(bool can_assign);
  void Literal(bool can_assign);
  void ParsePrecedence(Precedence precedence);
  uint8_t IdentifierConstant(const Token &name);
  void AddLocal(const Token &name);
  void DeclareVariable();
  uint8_t ParseVariable(std::string_view error_message);
  void MarkInitialized();
  void DefineVariable(uint8_t global);
  uint8_t ArgumentList();
  void And(bool can_assign);
  void Or(bool can_assign);
  ParseRule &GetRule(TokenType type);
  void EmitConstant(Value value);
  void PatchJump(int offset);
  uint8_t MakeConstant(Value value);
  int ResolveLocal(const Token &name);
  int AddUpvalue(uint8_t index, bool is_local);
  int ResolveUpvalue(const Token &name);
  Chunk &CurrentChunk();

  static Impl *current_compiler_;
  static ClassCompiler *current_class_;
  ObjFunction *function_ = nullptr;
  FunctionType type_;
  Token current_;
  Token previous_;
  static bool had_error_;
  static bool panic_mode_;
  InternTable &intern_table_;
  Local locals_[kUint8tCount];
  int local_count_ = 0;
  int scope_depth_ = 0;
  static std::optional<Scanner> scanner_;
  Impl *enclosing_ = nullptr;
  Upvalue upvalues_[kUint8tCount];
};

bool Compiler::Impl::had_error_ = false;
bool Compiler::Impl::panic_mode_ = false;
Compiler::Impl *Compiler::Impl::current_compiler_ = nullptr;
ClassCompiler *Compiler::Impl::current_class_ = nullptr;

std::optional<Scanner> Compiler::Impl::scanner_ = std::nullopt;

// When creating a compiler externally, we always want to use type `SCRIPT`.
Compiler::Compiler(InternTable &intern_table, ObjFunction &function)
    : impl_(std::make_unique<Impl>(intern_table, function,
                                   Impl::FunctionType::SCRIPT)) {}

Compiler::~Compiler() = default;

bool Compiler::Compile(std::string_view source) {
  return impl_->Compile(source);
}

std::vector<Obj *> Compiler::GetRoots() { return impl_->GetRoots(); }

Compiler::Impl::Impl(InternTable &intern_table, ObjFunction &function,
                     FunctionType type)
    : intern_table_(intern_table), type_(type) {
  current_compiler_ = this;
  function_ = &function;

  Local &local = locals_[local_count_];
  ++local_count_;
  local.depth = 0;
  local.is_captured = false;
  if (type != FunctionType::FUNCTION) {
    local.name = Token{
        .type = TokenType::IDENTIFIER,
        .lexeme = "this",
        .line = -1,
    };
  } else {
    local.name = Token{
        .type = TokenType::IDENTIFIER,
        .lexeme = "",
        .line = -1,
    };
  }
}

bool Compiler::Impl::Compile(std::string_view source) {
  scanner_ = Scanner(source);
  had_error_ = false;
  panic_mode_ = false;
  Advance();

  while (!Match(TokenType::TOKEN_EOF)) {
    Declaration();
  }

  End();
  return !had_error_;
}

std::vector<Obj *> Compiler::Impl::GetRoots() {
  std::vector<Obj *> results;
  Impl *compiler = current_compiler_;
  while (compiler != nullptr) {
    if (compiler->function_ != nullptr) {
      results.push_back(compiler->function_);
    }
    compiler = compiler->enclosing_;
  }
  return results;
}

absl::flat_hash_map<TokenType, Compiler::Impl::ParseRule>
    Compiler::Impl::rules_ = {
        {TokenType::LEFT_PAREN,
         {&Compiler::Impl::Grouping, &Compiler::Impl::Call, Precedence::CALL}},
        {TokenType::RIGHT_PAREN, {nullptr, nullptr, Precedence::NONE}},
        {
            TokenType::LEFT_BRACE,
            {nullptr, nullptr, Precedence::NONE},
        },
        {TokenType::RIGHT_BRACE, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::COMMA, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::DOT, {nullptr, &Compiler::Impl::Dot, Precedence::CALL}},
        {TokenType::MINUS,
         {&Compiler::Impl::Unary, &Compiler::Impl::Binary, Precedence::TERM}},
        {TokenType::PLUS, {nullptr, &Compiler::Impl::Binary, Precedence::TERM}},
        {TokenType::SEMICOLON, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::SLASH,
         {nullptr, &Compiler::Impl::Binary, Precedence::FACTOR}},
        {TokenType::STAR,
         {nullptr, &Compiler::Impl::Binary, Precedence::FACTOR}},
        {TokenType::BANG, {&Compiler::Impl::Unary, nullptr, Precedence::NONE}},
        {TokenType::BANG_EQUAL,
         {nullptr, &Compiler::Impl::Binary, Precedence::EQUALITY}},
        {TokenType::EQUAL, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::EQUAL_EQUAL,
         {nullptr, &Compiler::Impl::Binary, Precedence::EQUALITY}},
        {TokenType::GREATER,
         {nullptr, &Compiler::Impl::Binary, Precedence::COMPARISON}},
        {TokenType::GREATER_EQUAL,
         {nullptr, &Compiler::Impl::Binary, Precedence::COMPARISON}},
        {TokenType::LESS,
         {nullptr, &Compiler::Impl::Binary, Precedence::COMPARISON}},
        {TokenType::LESS_EQUAL,
         {nullptr, &Compiler::Impl::Binary, Precedence::COMPARISON}},
        {TokenType::IDENTIFIER,
         {&Compiler::Impl::Variable, nullptr, Precedence::NONE}},
        {TokenType::STRING,
         {&Compiler::Impl::String, nullptr, Precedence::NONE}},
        {TokenType::NUMBER,
         {&Compiler::Impl::Number, nullptr, Precedence::NONE}},
        {TokenType::AND, {nullptr, &Compiler::Impl::And, Precedence::AND}},
        {TokenType::CLASS, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::ELSE, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::FALSE,
         {&Compiler::Impl::Literal, nullptr, Precedence::NONE}},
        {TokenType::FOR, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::FUN, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::IF, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::NIL, {&Compiler::Impl::Literal, nullptr, Precedence::NONE}},
        {TokenType::OR, {nullptr, &Compiler::Impl::Or, Precedence::OR}},
        {TokenType::PRINT, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::RETURN, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::SUPER, {&Compiler::Impl::Super, nullptr, Precedence::NONE}},
        {TokenType::THIS, {&Compiler::Impl::This, nullptr, Precedence::NONE}},
        {TokenType::TRUE,
         {&Compiler::Impl::Literal, nullptr, Precedence::NONE}},
        {TokenType::VAR, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::WHILE, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::ERROR, {nullptr, nullptr, Precedence::NONE}},
        {TokenType::TOKEN_EOF, {nullptr, nullptr, Precedence::NONE}},
};

void Compiler::Impl::Advance() {
  previous_ = current_;

  for (;;) {
    current_ = scanner_->ScanToken();
    if (current_.type != TokenType::ERROR) {
      break;
    }

    ErrorAtCurrent(current_.lexeme);
  }
}

void Compiler::Impl::Consume(TokenType type, std::string_view message) {
  if (current_.type == type) {
    Advance();
    return;
  }

  ErrorAtCurrent(message);
}

bool Compiler::Impl::Match(TokenType type) {
  if (!Check(type)) {
    return false;
  }
  Advance();
  return true;
}

bool Compiler::Impl::Check(TokenType type) { return current_.type == type; }

void Compiler::Impl::ErrorAtCurrent(std::string_view message) {
  ErrorAt(current_, message);
}

void Compiler::Impl::Error(std::string_view message) {
  ErrorAt(previous_, message);
}

void Compiler::Impl::ErrorAt(const Token &token, std::string_view message) {
  if (panic_mode_) {
    return;
  }
  panic_mode_ = true;
  std::cerr << "[line " << token.line << "] Error";

  if (token.type == TokenType::TOKEN_EOF) {
    std::cerr << " at end";
  } else if (token.type == TokenType::ERROR) {
  } else {
    std::cerr << " at '" << token.lexeme << "'";
  }
  std::cerr << ": " << message << std::endl;
  had_error_ = true;
}

void Compiler::Impl::End() {
  EmitReturn();

#ifdef DEBUG_PRINT_CODE
  if (!had_error_) {
    CurrentChunk().Disassemble(
        function_->Name() == nullptr ? "<script>" : function_->Name()->Value());
  }
#endif
}

void Compiler::Impl::BeginScope() { ++scope_depth_; }

void Compiler::Impl::EndScope() {
  --scope_depth_;

  while (local_count_ > 0 && locals_[local_count_ - 1].depth > scope_depth_) {
    if (locals_[local_count_ - 1].is_captured) {
      EmitByte(OpCode::CLOSE_UPVALUE);
    } else {
      EmitByte(OpCode::POP);
    }
    --local_count_;
  }
}

void Compiler::Impl::EmitReturn() {
  if (type_ == FunctionType::INITIALIZER) {
    EmitBytes(OpCode::GET_LOCAL, 0);
  } else {
    EmitByte(OpCode::NIL);
  }
  EmitByte(OpCode::RETURN);
}

void Compiler::Impl::Declaration() {
  if (Match(TokenType::CLASS)) {
    ClassDeclaration();
  } else if (Match(TokenType::FUN)) {
    FunDeclaration();
  } else if (Match(TokenType::VAR)) {
    VarDeclaration();
  } else {
    Statement();
  }

  if (panic_mode_) {
    Synchronize();
  }
}

void Compiler::Impl::FunDeclaration() {
  uint8_t global = ParseVariable("Expect function name.");
  MarkInitialized();
  Function(FunctionType::FUNCTION);
  DefineVariable(global);
}

void Compiler::Impl::ClassDeclaration() {
  Consume(TokenType::IDENTIFIER, "Expect class name.");
  Token class_name = previous_;
  uint8_t name_constant = IdentifierConstant(previous_);
  DeclareVariable();

  EmitBytes(OpCode::CLASS, name_constant);
  DefineVariable(name_constant);

  ClassCompiler class_compiler = {.enclosing = current_class_};
  current_class_ = &class_compiler;

  if (Match(TokenType::LESS)) {
    Consume(TokenType::IDENTIFIER, "Expect superclass name.");
    Variable(false);

    if (class_name.lexeme == previous_.lexeme) {
      Error("A class can't inherit from itself.");
    }

    BeginScope();
    AddLocal(SyntheticToken("super"));
    DefineVariable(0);

    NamedVariable(class_name, false);
    EmitByte(OpCode::INHERIT);
    class_compiler.has_superclass = true;
  }

  NamedVariable(class_name, false);
  Consume(TokenType::LEFT_BRACE, "Expect '{' before class body.");
  while (!Check(TokenType::RIGHT_BRACE) && !Check(TokenType::TOKEN_EOF)) {
    Method();
  }
  Consume(TokenType::RIGHT_BRACE, "Expect '}' after class body.");
  EmitByte(OpCode::POP);

  if (class_compiler.has_superclass) {
    EndScope();
  }

  current_class_ = current_class_->enclosing;
}

void Compiler::Impl::VarDeclaration() {
  uint8_t global = ParseVariable("Expect variable name.");

  if (Match(TokenType::EQUAL)) {
    Expression();
  } else {
    EmitByte(OpCode::NIL);
  }
  Consume(TokenType::SEMICOLON, "Expect ';' after variable declaration.");
  DefineVariable(global);
}

void Compiler::Impl::Statement() {
  if (Match(TokenType::PRINT)) {
    PrintStatement();
  } else if (Match(TokenType::FOR)) {
    ForStatement();
  } else if (Match(TokenType::IF)) {
    IfStatement();
  } else if (Match(TokenType::RETURN)) {
    ReturnStatement();
  } else if (Match(TokenType::WHILE)) {
    WhileStatement();
  } else if (Match(TokenType::LEFT_BRACE)) {
    BeginScope();
    Block();
    EndScope();
  } else {
    ExpressionStatement();
  }
}

void Compiler::Impl::PrintStatement() {
  Expression();
  Consume(TokenType::SEMICOLON, "Expect ';' after value.");
  EmitByte(OpCode::PRINT);
}

void Compiler::Impl::ReturnStatement() {
  if (type_ == FunctionType::SCRIPT) {
    Error("Can't return from top-level code.");
  }

  if (Match(TokenType::SEMICOLON)) {
    EmitReturn();
  } else {
    if (type_ == FunctionType::INITIALIZER) {
      Error("Can't return a value from an initializer.");
    }

    Expression();
    Consume(TokenType::SEMICOLON, "Expect ';' after return value.");
    EmitByte(OpCode::RETURN);
  }
}

void Compiler::Impl::WhileStatement() {
  auto loop_start = CurrentChunk().Size();
  Consume(TokenType::LEFT_PAREN, "Expect '(' after 'while'.");
  Expression();
  Consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");

  auto exit_jump = EmitJump(OpCode::JUMP_IF_FALSE);
  EmitByte(OpCode::POP);
  Statement();
  EmitLoop(loop_start);

  PatchJump(exit_jump);
  EmitByte(OpCode::POP);
}

void Compiler::Impl::Synchronize() {
  panic_mode_ = false;

  while (current_.type != TokenType::TOKEN_EOF) {
    if (previous_.type == TokenType::SEMICOLON) {
      return;
    }
    switch (current_.type) {
    case TokenType::CLASS:
    case TokenType::FUN:
    case TokenType::VAR:
    case TokenType::FOR:
    case TokenType::IF:
    case TokenType::WHILE:
    case TokenType::PRINT:
    case TokenType::RETURN: {
      return;
    }
    default: {
      break;
    }
    }

    Advance();
  }
}

void Compiler::Impl::ExpressionStatement() {
  Expression();
  Consume(TokenType::SEMICOLON, "Expect ';' after expression.");
  EmitByte(OpCode::POP);
}

void Compiler::Impl::ForStatement() {
  BeginScope();
  Consume(TokenType::LEFT_PAREN, "Expect '(' after 'for'.");
  if (Match(TokenType::SEMICOLON)) {
    // No initializer.
  } else if (Match(TokenType::VAR)) {
    VarDeclaration();
  } else {
    ExpressionStatement();
  }

  auto loop_start = CurrentChunk().Size();
  std::optional<int> exit_jump = std::nullopt;
  if (!Match(TokenType::SEMICOLON)) {
    Expression();
    Consume(TokenType::SEMICOLON, "Expect ';' after loop condition.");

    exit_jump = EmitJump(OpCode::JUMP_IF_FALSE);
    EmitByte(OpCode::POP);
  }

  if (!Match(TokenType::RIGHT_PAREN)) {
    auto body_jump = EmitJump(OpCode::JUMP);
    auto increment_start = CurrentChunk().Size();
    Expression();
    EmitByte(OpCode::POP);
    Consume(TokenType::RIGHT_PAREN, "Expect ')' after for clauses.");

    EmitLoop(loop_start);
    loop_start = increment_start;
    PatchJump(body_jump);
  }

  Statement();
  EmitLoop(loop_start);

  if (exit_jump != std::nullopt) {
    PatchJump(*exit_jump);
    EmitByte(OpCode::POP);
  }

  EndScope();
}

void Compiler::Impl::IfStatement() {
  Consume(TokenType::LEFT_PAREN, "Expect '(' after 'if'.");
  Expression();
  Consume(TokenType::RIGHT_PAREN, "Expect ')' after condition.");

  auto then_jump = EmitJump(OpCode::JUMP_IF_FALSE);
  EmitByte(OpCode::POP);
  Statement();

  auto else_jump = EmitJump(OpCode::JUMP);

  PatchJump(then_jump);
  EmitByte(OpCode::POP);

  if (Match(TokenType::ELSE)) {
    Statement();
  }
  PatchJump(else_jump);
}

void Compiler::Impl::Expression() { ParsePrecedence(Precedence::ASSIGNMENT); }

void Compiler::Impl::Block() {
  while (!Check(TokenType::RIGHT_BRACE) && !Check(TokenType::TOKEN_EOF)) {
    Declaration();
  }

  Consume(TokenType::RIGHT_BRACE, "Expect '}' after block.");
}

void Compiler::Impl::Function(FunctionType type) {
  // Note that we may need to keep track of the parent compiler so we can
  // determine closure references at some point in the future.
  //
  // FIXME: Currently we share a lexer by making it a static member of `Impl`.
  //
  // This should be straightforward to clean up by propagating the lexer
  // via the constructor. We'll see :)
  ObjFunction *function = new ObjFunction();
  Impl compiler(intern_table_, *function, type);
  // FIXME: There should be a cleaner way to initialize the new compiler.
  compiler.current_ = current_;
  compiler.enclosing_ = this;
  function->SetName(*intern_table_.CreateString(previous_.lexeme));
  compiler.BeginScope();
  compiler.Consume(TokenType::LEFT_PAREN, "Expect '(' after function name.");
  if (!compiler.Check(TokenType::RIGHT_PAREN)) {
    int arity = 0;
    do {
      ++arity;
      if (arity > 255) {
        compiler.ErrorAtCurrent("Can't have more than 255 parameters.");
      }
      uint8_t constant = compiler.ParseVariable("Expect parameter name.");
      compiler.DefineVariable(constant);
    } while (compiler.Match(TokenType::COMMA));
    compiler.function_->SetArity(arity);
  }
  compiler.Consume(TokenType::RIGHT_PAREN, "Expect ')' after parameters.");
  compiler.Consume(TokenType::LEFT_BRACE, "Expect '{' before function body.");
  compiler.Block();
  compiler.End();

  EmitBytes(OpCode::CLOSURE, MakeConstant(function));

  for (int i = 0; i < function->UpvalueCount(); ++i) {
    EmitByte(compiler.upvalues_[i].is_local ? 1 : 0);
    EmitByte(compiler.upvalues_[i].index);
  }

  // FIXME: Yep, this is a mess.
  previous_ = compiler.previous_;
  current_ = compiler.current_;
}

void Compiler::Impl::Method() {
  Consume(TokenType::IDENTIFIER, "Expect method name.");
  uint8_t constant = IdentifierConstant(previous_);

  FunctionType type = FunctionType::METHOD;
  if (previous_.lexeme == "init") {
    type = FunctionType::INITIALIZER;
  }
  Function(type);
  EmitBytes(OpCode::METHOD, constant);
}

int Compiler::Impl::EmitJump(OpCode instruction) {
  EmitByte(instruction);
  EmitByte(0xff);
  EmitByte(0xff);
  return CurrentChunk().Size() - 2;
}

void Compiler::Impl::EmitLoop(int loop_start) {
  EmitByte(OpCode::LOOP);

  int offset = CurrentChunk().Size() - loop_start + 2;
  if (offset > std::numeric_limits<uint16_t>::max()) {
    Error("Loop body too large.");
  }

  EmitByte((offset >> 8) & 0xff);
  EmitByte(offset & 0xff);
}

Compiler::Impl::ParseRule &Compiler::Impl::GetRule(TokenType type) {
  return Compiler::Impl::rules_[type];
}

void Compiler::Impl::Number(bool can_assign) {
  std::string buf(previous_.lexeme);
  double value = strtod(buf.c_str(), nullptr);
  EmitConstant(value);
}

void Compiler::Impl::String(bool can_assign) {
  EmitConstant(intern_table_.CreateString(
      previous_.lexeme.substr(1, previous_.lexeme.size() - 2)));
}

void Compiler::Impl::NamedVariable(const Token &name, bool can_assign) {
  OpCode get_op, set_op;
  int arg = ResolveLocal(name);
  if (arg != -1) {
    get_op = OpCode::GET_LOCAL;
    set_op = OpCode::SET_LOCAL;
  } else if ((arg = ResolveUpvalue(name)) != -1) {
    get_op = OpCode::GET_UPVALUE;
    set_op = OpCode::SET_UPVALUE;
  } else {
    arg = IdentifierConstant(name);
    get_op = OpCode::GET_GLOBAL;
    set_op = OpCode::SET_GLOBAL;
  }

  if (can_assign && Match(TokenType::EQUAL)) {
    Expression();
    EmitBytes(set_op, arg);
  } else {
    EmitBytes(get_op, arg);
  }
}

void Compiler::Impl::Variable(bool can_assign) {
  NamedVariable(previous_, can_assign);
}

void Compiler::Impl::This(bool can_assign) {
  if (current_class_ == nullptr) {
    Error("Can't use 'this' outside of a class.");
    return;
  }

  Variable(false);
}

void Compiler::Impl::Super(bool can_assign) {
  if (current_class_ == nullptr) {
    Error("Can't use 'super' outside of a class.");
  } else if (!current_class_->has_superclass) {
    Error("Can't use 'super' in a class with no superclass.");
  }

  Consume(TokenType::DOT, "Expect '.' after 'super'.");
  Consume(TokenType::IDENTIFIER, "Expect superclass method name.");
  uint8_t name = IdentifierConstant(previous_);

  NamedVariable(SyntheticToken("this"), false);
  if (Match(TokenType::LEFT_PAREN)) {
    uint8_t arg_count = ArgumentList();
    NamedVariable(SyntheticToken("super"), false);
    EmitBytes(OpCode::SUPER_INVOKE, name);
    EmitByte(arg_count);
  } else {
    NamedVariable(SyntheticToken("super"), false);
    EmitBytes(OpCode::GET_SUPER, name);
  }
}

void Compiler::Impl::Grouping(bool can_assign) {
  Expression();
  Consume(TokenType::RIGHT_PAREN, "Expect ')' after expression.");
}

void Compiler::Impl::Unary(bool can_assign) {
  TokenType operator_type = previous_.type;

  ParsePrecedence(Precedence::UNARY);

  switch (operator_type) {
  case TokenType::BANG: {
    EmitByte(OpCode::NOT);
    break;
  }
  case TokenType::MINUS: {
    EmitByte(OpCode::NEGATE);
    break;
  }
  default: {
    // Unreachable.
    return;
  }
  }
}

void Compiler::Impl::Binary(bool can_assign) {
  TokenType operator_type = previous_.type;
  ParseRule &rule = GetRule(operator_type);
  ParsePrecedence(
      static_cast<Precedence>(static_cast<int>(rule.precedence) + 1));

  switch (operator_type) {
  case TokenType::BANG_EQUAL: {
    EmitBytes(OpCode::EQUAL, OpCode::NOT);
    break;
  }
  case TokenType::EQUAL_EQUAL: {
    EmitByte(OpCode::EQUAL);
    break;
  }
  case TokenType::GREATER: {
    EmitByte(OpCode::GREATER);
    break;
  }
  case TokenType::GREATER_EQUAL: {
    EmitBytes(OpCode::LESS, OpCode::NOT);
    break;
  }
  case TokenType::LESS: {
    EmitByte(OpCode::LESS);
    break;
  }
  case TokenType::LESS_EQUAL: {
    EmitBytes(OpCode::GREATER, OpCode::NOT);
    break;
  }
  case TokenType::PLUS:
    EmitByte(OpCode::ADD);
    break;
  case TokenType::MINUS:
    EmitByte(OpCode::SUBTRACT);
    break;
  case TokenType::STAR:
    EmitByte(OpCode::MULTIPLY);
    break;
  case TokenType::SLASH:
    EmitByte(OpCode::DIVIDE);
    break;
  default:
    // Unreachable.
    return;
  }
}

void Compiler::Impl::Call(bool can_assign) {
  uint8_t arg_count = ArgumentList();
  EmitBytes(OpCode::CALL, arg_count);
}

void Compiler::Impl::Dot(bool can_assign) {
  Consume(TokenType::IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = IdentifierConstant(previous_);

  if (can_assign && Match(TokenType::EQUAL)) {
    Expression();
    EmitBytes(OpCode::SET_PROPERTY, name);
  } else if (Match(TokenType::LEFT_PAREN)) {
    uint8_t arg_count = ArgumentList();
    EmitBytes(OpCode::INVOKE, name);
    EmitByte(arg_count);
  } else {
    EmitBytes(OpCode::GET_PROPERTY, name);
  }
}

void Compiler::Impl::Literal(bool can_assign) {
  switch (previous_.type) {
  case TokenType::FALSE:
    EmitByte(OpCode::FALSE);
    break;
  case TokenType::NIL:
    EmitByte(OpCode::NIL);
    break;
  case TokenType::TRUE:
    EmitByte(OpCode::TRUE);
    break;
  default:
    return; // Unreachable.
  }
}

void Compiler::Impl::ParsePrecedence(Precedence precedence) {
  Advance();
  auto &prefix_rule = GetRule(previous_.type).prefix;
  if (prefix_rule == nullptr) {
    Error("Expect expression.");
    return;
  }

  bool can_assign = precedence <= Precedence::ASSIGNMENT;
  prefix_rule(this, can_assign);

  while (precedence <= GetRule(current_.type).precedence) {
    Advance();
    auto &infix_rule = GetRule(previous_.type).infix;
    infix_rule(this, can_assign);
  }

  if (can_assign && Match(TokenType::EQUAL)) {
    Error("Invalid assignment target.");
  }
}

uint8_t Compiler::Impl::IdentifierConstant(const Token &name) {
  return MakeConstant(intern_table_.CreateString(name.lexeme));
}

void Compiler::Impl::AddLocal(const Token &name) {
  if (local_count_ == kUint8tCount) {
    Error("Too many local variables in function.");
    return;
  }
  Local &local = locals_[local_count_];
  ++local_count_;
  local.name = name;
  local.depth = -1;
  local.is_captured = false;
}

void Compiler::Impl::DeclareVariable() {
  if (scope_depth_ == 0) {
    return;
  }
  Token &name = previous_;
  for (int i = local_count_ - 1; i >= 0; --i) {
    const Local &local = locals_[i];
    if (local.depth != -1 && local.depth < scope_depth_) {
      break;
    }

    if (name.lexeme == local.name.lexeme) {
      Error("Already a variable with this name in this scope.");
    }
  }

  AddLocal(previous_);
}

uint8_t Compiler::Impl::ParseVariable(std::string_view error_message) {
  Consume(TokenType::IDENTIFIER, error_message);

  DeclareVariable();
  if (scope_depth_ > 0) {
    return 0;
  }

  return IdentifierConstant(previous_);
}

void Compiler::Impl::MarkInitialized() {
  if (scope_depth_ == 0) {
    return;
  }
  locals_[local_count_ - 1].depth = scope_depth_;
}

void Compiler::Impl::DefineVariable(uint8_t global) {
  if (scope_depth_ > 0) {
    MarkInitialized();
    return;
  }

  EmitBytes(OpCode::DEFINE_GLOBAL, global);
}

uint8_t Compiler::Impl::ArgumentList() {
  uint8_t arg_count = 0;
  if (!Check(TokenType::RIGHT_PAREN)) {
    do {
      Expression();
      if (arg_count == 255) {
        Error("Can't have more than 255 arguments.");
      }
      ++arg_count;
    } while (Match(TokenType::COMMA));
  }
  Consume(TokenType::RIGHT_PAREN, "Expect ')' after arguments.");
  return arg_count;
}

void Compiler::Impl::And(bool can_assign) {
  auto end_jump = EmitJump(OpCode::JUMP_IF_FALSE);

  EmitByte(OpCode::POP);
  ParsePrecedence(Precedence::AND);

  PatchJump(end_jump);
}

void Compiler::Impl::Or(bool can_assign) {
  auto else_jump = EmitJump(OpCode::JUMP_IF_FALSE);
  auto end_jump = EmitJump(OpCode::JUMP);
  PatchJump(end_jump);

  PatchJump(else_jump);
  EmitByte(OpCode::POP);

  ParsePrecedence(Precedence::OR);
  PatchJump(end_jump);
}

void Compiler::Impl::EmitConstant(Value value) {
  EmitBytes(OpCode::CONSTANT, MakeConstant(value));
}

void Compiler::Impl::PatchJump(int offset) {
  auto jump = CurrentChunk().Size() - offset - 2;

  if (jump > std::numeric_limits<uint16_t>::max()) {
    Error("Too much code to jump over.");
  }

  CurrentChunk()[offset] = (jump >> 8) & 0xff;
  CurrentChunk()[offset + 1] = jump & 0xff;
}

uint8_t Compiler::Impl::MakeConstant(Value value) {
  int constant = CurrentChunk().AddConstant(value);
  if (constant > std::numeric_limits<uint8_t>::max()) {
    Error("Too many constants in one chunk.");
    return 0;
  }

  return static_cast<uint8_t>(constant);
}

int Compiler::Impl::ResolveLocal(const Token &name) {
  for (int i = local_count_ - 1; i >= 0; --i) {
    const Local &local = locals_[i];
    if (name.lexeme == local.name.lexeme) {
      if (local.depth == -1) {
        Error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }

  return -1;
}

int Compiler::Impl::AddUpvalue(uint8_t index, bool is_local) {
  int upvalue_count = function_->UpvalueCount();

  if (upvalue_count == kUint8tCount) {
    Error("Too many closure variables in function.");
    return 0;
  }

  for (int i = 0; i < upvalue_count; ++i) {
    const Upvalue &upvalue = upvalues_[i];
    if (upvalue.index == index && upvalue.is_local == is_local) {
      return i;
    }
  }

  upvalues_[upvalue_count].is_local = is_local;
  upvalues_[upvalue_count].index = index;
  function_->IncrementUpvalueCount();
  return upvalue_count;
}

int Compiler::Impl::ResolveUpvalue(const Token &name) {
  if (enclosing_ == nullptr) {
    return -1;
  }

  int local = enclosing_->ResolveLocal(name);
  if (local != -1) {
    enclosing_->locals_[local].is_captured = true;
    return AddUpvalue(static_cast<uint8_t>(local), true);
  }

  int upvalue = enclosing_->ResolveUpvalue(name);
  if (upvalue != -1) {
    return AddUpvalue(static_cast<uint8_t>(upvalue), false);
  }

  return -1;
}

Chunk &Compiler::Impl::CurrentChunk() { return function_->GetChunk(); }
