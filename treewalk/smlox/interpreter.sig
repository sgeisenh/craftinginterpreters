signature INTERPRETER =
  sig
    val interpret : Environment.t
                      -> Parser.statement Common.annotated list
                      -> unit
  end
