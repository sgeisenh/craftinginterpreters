structure JSLox = SmLox(struct
  fun print (s : string) =
    JsCore.call1 ("console.log", JsCore.string, JsCore.unit) s
end)

val () = _export("runProgram", JSLox.runProgram)
