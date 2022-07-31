signature INTERPRETER =
  sig
    val interpret: Environment.t -> Parser.statement list -> unit
  end
