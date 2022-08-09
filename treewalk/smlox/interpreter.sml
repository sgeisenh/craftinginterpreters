structure Interpreter =
  struct
    open Parser

    exception ReturnExn of LoxValue.t

    fun evaluateExpr environment expr =
      let val {value = expr, ...} = expr in
        case expr of
          Assign ((ident, level), expr) =>
            let val result = evaluateExpr environment expr in
              (case level of
                 NONE => Environment.assign environment (ident, result)
               | SOME level =>
                   Environment.assignTo environment (ident, result) level);
              result
            end
        | Binary (binOp, left, right) =>
            let
              val left' = evaluateExpr environment left
              val right' = evaluateExpr environment right
            in
              (case binOp of
                 Dot => raise Fail "Unimplemented"
               | Minus => LoxValue.minus (left', right')
               | Plus => LoxValue.plus (left', right')
               | Slash => LoxValue.divides (left', right')
               | Star => LoxValue.times (left', right')
               | BangEqual => LoxValue.neq (left', right')
               | EqualEqual => LoxValue.eq (left', right')
               | Greater => LoxValue.greater (left', right')
               | GreaterEqual => LoxValue.greaterEq (left', right')
               | Less => LoxValue.less (left', right')
               | LessEqual => LoxValue.lessEq (left', right'))
            end
        | Call (callee, arguments) =>
            let
              val callee = evaluateExpr environment callee
              val arguments = map (evaluateExpr environment) arguments
            in
              LoxValue.call (callee, arguments) handle ReturnExn value => value
            end
        | Grouping expr' => evaluateExpr environment expr'
        | Literal literal =>
            (case literal of
               Number r => LoxValue.Number r
             | String s => LoxValue.String s
             | True => LoxValue.Boolean true
             | False => LoxValue.Boolean false
             | Nil => LoxValue.Nil)
        | Unary (unOp, expr') =>
            let val expr' = evaluateExpr environment expr' in
              case unOp of
                Bang => LoxValue.logicalNot expr'
              | Negative => LoxValue.negate expr'
            end
        | Variable (ident, binding) =>
            (case binding of
               NONE => Environment.get environment ident
             | SOME level => Environment.getFrom environment ident level)
        | Logical (Parser.Or, left, right) =>
            let val left = evaluateExpr environment left in
              if LoxValue.isTruthy left then
                left
              else
                evaluateExpr environment right
            end
        | Logical (Parser.And, left, right) =>
            let val left = evaluateExpr environment left in
              if LoxValue.isTruthy left then
                evaluateExpr environment right
              else
                left
            end
        | Get (obj, ident) =>
            let
              val obj = evaluateExpr environment obj
              (* TODO: This doesn't quite work *)
              val () = Environment.declare environment ("this", obj)
            in
              LoxValue.get obj ident
            end
        | Set (obj, ident, value) =>
            let
              val obj = evaluateExpr environment obj
              val value = evaluateExpr environment value
            in
              (LoxValue.set obj ident value; value)
            end
        | This binding =>
            (case binding of
               NONE => Environment.get environment "this"
             | SOME level => Environment.getFrom environment "this" level)
      end

    fun evaluateStatement environment print ogStatement =
      let val {value = statement, ...} = ogStatement in
        case statement of
          Block statements =>
            List.app
              (evaluateStatement (Environment.makeNested environment) print)
              statements
        | Class (name, methods) =>
            let
              val methodTable = StringTable.mkTable (256, Fail "Unknown method")
              fun createMethod method =
                let val {value, ...} = method in
                  case value of
                    Function function_info =>
                      let
                        val environment = Environment.makeNested environment
                        val (name, function) =
                          createFunction environment function_info
                      in
                        ( name
                        , fn this =>
                            ( Environment.declare environment ("this", this)
                            ; function
                            )
                        )
                      end
                  | _ => raise Fail "Non-method class contents?"
                end
              val () =
                app ((StringTable.insert methodTable) o createMethod) methods
            in
              Environment.declare environment
                (name, LoxValue.Class (name, methodTable))
            end
        | Expression expr => (evaluateExpr environment expr; ())
        | Function function_info =>
            let
              val (name, function) = createFunction environment function_info
            in
              Environment.declare environment (name, function)
            end
        | If (condition, thenBranch, elseBranch) =>
            let val conditionResult = evaluateExpr environment condition in
              if LoxValue.isTruthy conditionResult then
                evaluateStatement environment print thenBranch
              else
                Option.app (evaluateStatement environment print) elseBranch
            end
        | While (condition, body) =>
            let val conditionResult = evaluateExpr environment condition in
              if LoxValue.isTruthy conditionResult then
                ( evaluateStatement environment print body
                ; evaluateStatement environment print ogStatement
                )
              else
                ()
            end
        | Print expr =>
            let val result = evaluateExpr environment expr in
              print (LoxValue.toString result ^ "\n")
            end
        | Return expr =>
            let val result = evaluateExpr environment expr in
              raise ReturnExn result
            end
        | Var (ident, expr) =>
            let val result = evaluateExpr environment expr in
              Environment.declare environment (ident, result)
            end
      end
    and createFunction environment (name, parameters, body, kind) =
      let
        fun function arguments =
          if List.length arguments <> List.length parameters then
            raise Fail "arity"
          else
            let val newEnvironment = Environment.makeNested environment in
              ( List.app (Environment.declare newEnvironment)
                  (ListPair.zip (parameters, arguments))
              ; List.app (evaluateStatement newEnvironment print) body
              ; LoxValue.Nil
              )
            end
      in
        (name, LoxValue.Function (name, function))
      end

    fun interpret environment print =
      List.app (evaluateStatement environment print)
  end
