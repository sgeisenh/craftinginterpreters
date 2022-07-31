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
      Assign of ((string * int option) * expr Common.annotated)
    | Binary of
               (binary_operator * expr Common.annotated * expr Common.annotated)
    | Call of (expr Common.annotated * (expr Common.annotated) list)
    | Grouping of expr Common.annotated
    | Literal of literal
    | Logical of
                (logical_operator * expr Common.annotated * expr Common.annotated)
    | Unary of (unary_operator * expr Common.annotated)
    | Variable of string * int option
    datatype statement =
      Block of (statement Common.annotated) list
    | Expression of expr Common.annotated
    | Function of (string * string list * (statement Common.annotated) list)
    | If of
           (expr Common.annotated * statement Common.annotated * (statement Common.annotated) option)
    | While of (expr Common.annotated * statement Common.annotated)
    | Print of expr Common.annotated
    | Return of expr Common.annotated
    | Var of (string * expr Common.annotated)

    val parse : (Scanner.token Common.annotated) list -> ( (statement Common.annotated) list
                                                         , Common.error list
                                                         ) Common.result
  end
