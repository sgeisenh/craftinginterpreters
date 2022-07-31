signature BINDING =
  sig
    val attachBindings : string list -> Parser.statement Common.annotated list -> Parser.statement Common.annotated list
  end
