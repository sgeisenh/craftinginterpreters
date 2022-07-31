signature BINDING =
  sig
    val attachBindings : string list
                           -> Parser.statement Common.annotated list
                           -> ( Parser.statement Common.annotated list
                              , Common.error list
                              ) Common.result
  end
