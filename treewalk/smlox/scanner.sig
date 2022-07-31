signature SCANNER =
  sig
    datatype token =
      LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Identifier of string
    | String of string
    | Number of real
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | Eof

    type annotatedToken = {token : token, location : Common.source_location}

    val tokenEqual : (token * token) -> bool
    val tokenToString : token -> string

    type t
    val make : string -> t
    val scanTokens : t -> (annotatedToken list, Common.error list) Common.result
  end
