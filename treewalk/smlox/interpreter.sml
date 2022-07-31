structure Interpreter =
  struct
    open Parser

    fun evaluateExpr environment expr =
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
      | Variable ident => Environment.get environment ident
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

    fun evaluateStatement environment statement =
      case statement of
        Block statements =>
          List.app (evaluateStatement (Environment.makeNested environment))
            statements
      | Expression expr => (evaluateExpr environment expr; ())
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
              ; evaluateStatement environment (While (condition, body))
              )
            else
              ()
          end
      | Print expr =>
          let val result = evaluateExpr environment expr in
            print (LoxValue.toString result ^ "\n")
          end
      | Var (ident, expr) =>
          let val result = evaluateExpr environment expr in
            Environment.declare environment (ident, result)
          end

    fun interpret environment = List.app (evaluateStatement environment)
  end
