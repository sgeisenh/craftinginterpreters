structure Interpreter =
  struct
    open Parser

    exception ReturnExn of LoxValue.t

    fun evaluateExpr environment expr =
      let val {value = expr, ...} = expr in
        case expr of
          Assign (ident, expr) =>
            let val result = evaluateExpr environment expr in
              Environment.assign environment (ident, result); result
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
        | Variable(ident, binding) => Environment.get environment ident
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
      end

    fun evaluateStatement environment ogStatement =
      let val {value = statement, ...} = ogStatement in
        case statement of
          Block statements =>
            List.app (evaluateStatement (Environment.makeNested environment))
              statements
        | Expression expr => (evaluateExpr environment expr; ())
        | Function (name, parameters, body) =>
            let
              fun function arguments =
                if List.length arguments <> List.length parameters then
                  raise Fail "arity"
                else
                  let val newEnvironment = Environment.makeNested environment in
                    ( List.app (Environment.declare newEnvironment)
                        (ListPair.zip (parameters, arguments))
                    ; List.app (evaluateStatement newEnvironment) body
                    ; LoxValue.Nil
                    )
                  end
            in
              Environment.declare environment (name, LoxValue.Function function)
            end
        | If (condition, thenBranch, elseBranch) =>
            let val conditionResult = evaluateExpr environment condition in
              if LoxValue.isTruthy conditionResult then
                evaluateStatement environment thenBranch
              else
                Option.app (evaluateStatement environment) elseBranch
            end
        | While (condition, body) =>
            let val conditionResult = evaluateExpr environment condition in
              if LoxValue.isTruthy conditionResult then
                ( evaluateStatement environment body
                ; evaluateStatement environment ogStatement
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

    fun interpret environment = List.app (evaluateStatement environment)
  end
