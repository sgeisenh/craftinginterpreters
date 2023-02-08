#pragma once

#include <string_view>

#include "common.h"

enum class TokenType {
  // Single-character tokens.
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,
  // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,
  // Literals.
  IDENTIFIER,
  STRING,
  NUMBER,
  // Keywords.
  AND,
  CLASS,
  ELSE,
  FALSE,
  FOR,
  FUN,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,
  ERROR,
  TOKEN_EOF,
};

struct Token {
  TokenType type;
  std::string_view lexeme;
  int line;
};

class Scanner {
public:
  explicit Scanner(std::string_view source);

  Token ScanToken();

private:
  bool IsAtEnd();
  char Advance();
  char Peek();
  char PeekNext();
  bool Match(char expected);
  Token MakeToken(TokenType type);
  Token ErrorToken(std::string_view message);
  void SkipWhitespace();
  TokenType IdentifierType();
  Token Identifier();
  Token String();
  Token Number();
  TokenType CheckKeyword(int start, std::string_view rest, TokenType type);

  std::string_view source_;
  int start_ = 0;
  int current_ = 0;
  int line_ = 1;
};
