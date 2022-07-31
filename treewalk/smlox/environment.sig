signature ENVIRONMENT =
  sig
    type t

    exception UnknownVariable of string

    val make: unit -> t
    val makeNested: t -> t
    val declare: t -> (string * LoxValue.t) -> unit
    val get: t -> string -> LoxValue.t
    val assign: t -> (string * LoxValue.t) -> unit
  end
