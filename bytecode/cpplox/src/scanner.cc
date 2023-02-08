#include "scanner.h"

#include <string_view>

namespace {

bool IsAlpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool IsDigit(char c) { return c >= '0' && c <= '9'; }

} // namespace

Scanner::Scanner(std::string_view source) : source_(source) {}

Token Scanner::ScanToken() {
  SkipWhitespace();
  start_ = current_;

  if (IsAtEnd()) {
    return MakeToken(TokenType::TOKEN_EOF);
  }

  char c = Advance();
  if (IsDigit(c)) {
    return Number();
  }
  if (IsAlpha(c)) {
    return Identifier();
  }

  switch (c) {
  case '(':
    return MakeToken(TokenType::LEFT_PAREN);
  case ')':
    return MakeToken(TokenType::RIGHT_PAREN);
  case '{':
    return MakeToken(TokenType::LEFT_BRACE);
  case '}':
    return MakeToken(TokenType::RIGHT_BRACE);
  case ';':
    return MakeToken(TokenType::SEMICOLON);
  case ',':
    return MakeToken(TokenType::COMMA);
  case '.':
    return MakeToken(TokenType::DOT);
  case '-':
    return MakeToken(TokenType::MINUS);
  case '+':
    return MakeToken(TokenType::PLUS);
  case '/':
    return MakeToken(TokenType::SLASH);
  case '*':
    return MakeToken(TokenType::STAR);
  case '!':
    return MakeToken(Match('=') ? TokenType::BANG_EQUAL : TokenType::BANG);
  case '=':
    return MakeToken(Match('=') ? TokenType::EQUAL_EQUAL : TokenType::EQUAL);
  case '<':
    return MakeToken(Match('=') ? TokenType::LESS_EQUAL : TokenType::LESS);
  case '>':
    return MakeToken(Match('=') ? TokenType::GREATER_EQUAL
                                : TokenType::GREATER);
  case '"':
    return String();
  }

  return ErrorToken("Unexpected character.");
}

bool Scanner::IsAtEnd() { return current_ >= source_.size(); }

char Scanner::Advance() {
  char result = source_[current_];
  ++current_;
  return result;
}

char Scanner::Peek() { return source_[current_]; }

char Scanner::PeekNext() {
  if (IsAtEnd()) {
    return '\0';
  }
  return source_[current_ + 1];
}

bool Scanner::Match(char expected) {
  if (IsAtEnd()) {
    return false;
  }
  if (source_[current_] != expected) {
    return false;
  }
  ++current_;
  return true;
}

Token Scanner::MakeToken(TokenType type) {
  return Token{
      .type = type,
      .lexeme = source_.substr(start_, current_ - start_),
      .line = line_,
  };
}

Token Scanner::ErrorToken(std::string_view message) {
  return Token{
      .type = TokenType::ERROR,
      .lexeme = message,
      .line = line_,
  };
}

void Scanner::SkipWhitespace() {
  for (;;) {
    switch (Peek()) {
    case ' ':
    case '\r':
    case '\t': {
      Advance();
      break;
    }
    case '\n': {
      ++line_;
      Advance();
      break;
    }
    case '/': {
      if (PeekNext() == '/') {
        while (Peek() != '\n' && !IsAtEnd()) {
          Advance();
        }
      } else {
        return;
      }
      break;
    }
    default:
      return;
    }
  }
}

TokenType Scanner::CheckKeyword(int start, std::string_view rest,
                                TokenType type) {
  // This might be an overoptimization. We should be able to just take the slice
  // from [start_ + start, current_) and compare.
  if (current_ - start_ == start + rest.size() &&
      source_.substr(start_ + start, rest.size()) == rest) {
    return type;
  }
  return TokenType::IDENTIFIER;
}

TokenType Scanner::IdentifierType() {
  switch (source_[start_]) {
  case 'a':
    return CheckKeyword(1, "nd", TokenType::AND);
  case 'c':
    return CheckKeyword(1, "lass", TokenType::CLASS);
  case 'e':
    return CheckKeyword(1, "lse", TokenType::ELSE);
  case 'f':
    if (current_ - start_ > 1) {
      switch (source_[start_ + 1]) {
      case 'a':
        return CheckKeyword(2, "lse", TokenType::FALSE);
      case 'o':
        return CheckKeyword(2, "r", TokenType::FOR);
      case 'u':
        return CheckKeyword(2, "n", TokenType::FUN);
      }
    }
    break;
  case 'i':
    return CheckKeyword(1, "f", TokenType::IF);
  case 'n':
    return CheckKeyword(1, "il", TokenType::NIL);
  case 'o':
    return CheckKeyword(1, "r", TokenType::OR);
  case 'p':
    return CheckKeyword(1, "rint", TokenType::PRINT);
  case 'r':
    return CheckKeyword(1, "eturn", TokenType::RETURN);
  case 's':
    return CheckKeyword(1, "uper", TokenType::SUPER);
  case 't':
    if (current_ - start_ > 1) {
      switch (source_[start_ + 1]) {
      case 'h':
        return CheckKeyword(2, "is", TokenType::THIS);
      case 'r':
        return CheckKeyword(2, "ue", TokenType::TRUE);
      }
    }
    break;
  case 'v':
    return CheckKeyword(1, "ar", TokenType::VAR);
  case 'w':
    return CheckKeyword(1, "hile", TokenType::WHILE);
  }
  return TokenType::IDENTIFIER;
}

Token Scanner::Identifier() {
  while (IsAlpha(Peek()) || IsDigit(Peek())) {
    Advance();
  }
  return MakeToken(IdentifierType());
}

Token Scanner::String() {
  while (Peek() != '"' && !IsAtEnd()) {
    if (Peek() == '\n') {
      ++line_;
    }
    Advance();
  }

  if (IsAtEnd()) {
    return ErrorToken("Unterminated string.");
  }

  // The closing quote.
  Advance();
  return MakeToken(TokenType::STRING);
}

Token Scanner::Number() {
  while (IsDigit(Peek())) {
    Advance();
  }

  if (Peek() == '.' && IsDigit(PeekNext())) {
    // Consume the ".".
    Advance();

    while (IsDigit(Peek())) {
      Advance();
    }
  }

  return MakeToken(TokenType::NUMBER);
}
