signature LOX_VALUE =
  sig
    datatype t = Nil | Boolean of bool | Number of real | String of string

    exception RuntimeError of string

    val eq : (t * t) -> t
    val neq: (t * t) -> t
    val minus: (t * t) -> t
    val plus: (t * t) -> t
    val times: (t * t) -> t
    val divides: (t * t) -> t
    val greater: (t * t) -> t
    val greaterEq: (t * t) -> t
    val less: (t * t) -> t
    val lessEq: (t * t) -> t
    val negate: t -> t
    val logicalNot: t -> t

    val isTruthy: t -> bool

    val toString: t -> string
  end
