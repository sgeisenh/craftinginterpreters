signature INTERPRETER =
  sig
    val interpret : Environment.t
                      -> (string -> unit)
                      -> Parser.statement Common.annotated list
                      -> unit
  end
