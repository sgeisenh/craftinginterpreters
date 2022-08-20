functor SmLox (R : RUNTIME) =
  struct
    open Common

    val startTimeSecs = Time.toReal (Time.now ())

    val globals =
      [ ( "clock"
        , LoxValue.Function
            ( "native fn"
            , (fn _ :: _ => raise Fail "clock accepts 0 arguments"
                | _ =>
                  LoxValue.Number (Time.toReal (Time.now ()) - startTimeSecs))
            , LoxValue.getId ()
            )
        )
      ]

    fun run environment program =
      let
        val scanner = Scanner.make program
        val tokensOrErrors = Scanner.scanTokens scanner
        val astOrErrors = fmap Parser.parse tokensOrErrors
        val boundGlobalNames = map (fn (name, _) => name) globals
        val boundAstOrErrors =
          fmap (Resolver.attachBindings boundGlobalNames) astOrErrors
        val successOrFailure =
          fmap
            (fn ast => Success (Interpreter.interpret environment R.print ast))
            boundAstOrErrors
      in
        case successOrFailure of
          Failure errors =>
            (Common.print_errors program errors; raise Fail "Errors")
        | _ => ()
      end

    fun runProgram program = run (Environment.make globals) program
  end
