signature PARSER =
  sig
    datatype literal = Number of real | String of string | True | False | Nil
    datatype binary_operator =
      Dot
    | Minus
    | Plus
    | Slash
    | Star
    | BangEqual
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    datatype logical_operator = And | Or
    datatype unary_operator = Negative | Bang
    datatype expr =
      Assign of (string * expr)
    | Binary of (binary_operator * expr * expr)
    | Call of (expr * expr list)
    | Grouping of expr
    | Literal of literal
    | Logical of (logical_operator * expr * expr)
    | Unary of (unary_operator * expr)
    | Variable of string
    datatype statement =
      Block of statement list
    | Expression of expr
    | Function of (string * string list * statement list)
    | If of (expr * statement * statement option)
    | While of (expr * statement)
    | Print of expr
    | Return of expr
    | Var of (string * expr)

    val binOpToString : binary_operator -> string
    val unaryOpToString : unary_operator -> string
    val literalToString : literal -> string
    val exprToString : expr -> string

    val parse : Scanner.token list -> (statement list, Common.error list) Common.result
  end
