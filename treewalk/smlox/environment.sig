signature ENVIRONMENT =
  sig
    type t

    exception UnknownVariable of string

    val make : (string * LoxValue.t) list -> t
    val makeNested : t -> t
    val declare : t -> (string * LoxValue.t) -> unit
    val remove : t -> string -> unit
    val get : t -> string -> LoxValue.t
    val assign : t -> (string * LoxValue.t) -> unit
    val isGlobal : t -> bool

    val getJumps : t -> string -> int
    val getFrom : t -> string -> int -> LoxValue.t
    val assignTo : t -> (string * LoxValue.t) -> int -> unit
  end
