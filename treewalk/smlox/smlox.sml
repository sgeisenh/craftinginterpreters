val startTimeSecs = Time.toReal (Time.now ())

val globals =
  [ ( "clock"
    , LoxValue.Function
        (fn _ :: _ => raise Fail "clock accepts 0 arguments"
          | _ => LoxValue.Number (Time.toReal (Time.now ()) - startTimeSecs))
    )
  ]
fun run environment program =
  let
    val scanner = Scanner.make program
    val tokensOrErrors = Scanner.scanTokens scanner
    val justTokens =
      case tokensOrErrors of
        Result.Success tokens => tokens
      | Result.Failure failures =>
          ( (app
               (fn {message, line} =>
                  print
                    ("Error on line "
                     ^ Int.toString line
                     ^ ": "
                     ^ message
                     ^ "\n"))
               failures)
          ; raise Fail "Scanning error."
          )
    val ast = Parser.parse justTokens
  in
    Interpreter.interpret environment ast
  end

fun runFile filename =
  let val program = TextIO.input (TextIO.openIn filename) in
    run (Environment.make globals) program
  end

fun runPrompt () =
  let val environment = Environment.make globals in
    while true do
      let val maybeLine = TextIO.inputLine TextIO.stdIn in
        case maybeLine of
          SOME line => run environment line
        | NONE => OS.Process.exit OS.Process.success
      end
  end

val () =
  case CommandLine.arguments () of
    [] => runPrompt ()
  | s :: [] => runFile s
  | _ => (print "Usage: smlox [script]\n"; OS.Process.exit OS.Process.failure)
