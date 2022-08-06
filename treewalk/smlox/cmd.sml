open Common

structure SmLox = SmLox (struct
  val print = print
end)

fun runFile filename =
  let val program = TextIO.inputAll (TextIO.openIn filename) in
    SmLox.runProgram program
  end

fun runPrompt () =
  let val environment = Environment.make SmLox.globals in
    while true do
      let val maybeLine = TextIO.inputLine TextIO.stdIn in
        case maybeLine of
          SOME line => SmLox.run environment line
        | NONE => ()
      end
  end

val () =
  case CommandLine.arguments () of
    [] => runPrompt ()
  | s :: [] => runFile s
  | _ => (print "Usage: smlox [script]\n"; raise Fail "")
