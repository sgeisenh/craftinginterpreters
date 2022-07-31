structure Binding :> BINDING =
  struct
    open Parser
    val nonsenseValue = LoxValue.Nil

    fun declare context ident =
      let
        val global = Environment.isGlobal context
        fun isNew () =
          (Environment.getFrom context ident 0; false)
            handle Fail _ => true
      in
        if global orelse isNew () then
          Environment.declare context (ident, nonsenseValue)
        else
          raise
            Fail ("Redeclaring variable " ^ ident ^ " outside global scope.")
      end

    fun attachBindings' context = map (attachBinding context)
    and attachBinding context (statement : statement Common.annotated) =
      let
        val {value = statement, location} = statement
        val statement =
          case statement of
            Block statements =>
              let
                val context = Environment.makeNested context
                val statements = attachBindings' context statements
              in
                Block statements
              end
          | Expression expr =>
              let val expr = attachBindingToExpr context expr in
                Expression expr
              end
          | Function (ident, params, body) =>
              let
                val () = declare context ident
                val context = Environment.makeNested context
                val () = List.app (declare context) params
                val body = attachBindings' context body
              in
                Function (ident, params, body)
              end
          | If (cond, thn, els) =>
              let
                val cond = attachBindingToExpr context cond
                val thn = attachBinding context thn
                val els =
                  case els of
                    NONE => els
                  | SOME els => SOME (attachBinding context els)
              in
                If (cond, thn, els)
              end
          | While (cond, body) =>
              let
                val cond = attachBindingToExpr context cond
                val body = attachBinding context body
              in
                While (cond, body)
              end
          | Print expr =>
              let val expr = attachBindingToExpr context expr in Print expr end
          | Return expr =>
              let val expr = attachBindingToExpr context expr in Return expr end
          (* TODO: Make it an error to reference variable inside of initializer *)
          | Var (ident, expr) =>
              let
                val expr = attachBindingToExpr context expr
                val () = declare context ident
              in
                Var (ident, expr)
              end
      in
        {value = statement, location = location}
      end
    and attachBindingToExpr context expr =
      let
        val {value = expr, location} = expr
        val expr =
          case expr of
            Assign ((ident, _), expr) =>
              let
                val expr = attachBindingToExpr context expr
                val binding = Environment.getJumps context ident
              in
                Assign ((ident, SOME binding), expr)
              end
          | Binary (operator, left, right) =>
              let
                val left = attachBindingToExpr context left
                val right = attachBindingToExpr context right
              in
                Binary (operator, left, right)
              end
          | Call (callee, args) =>
              let
                val callee = attachBindingToExpr context callee
                val args = map (attachBindingToExpr context) args
              in
                Call (callee, args)
              end
          | Grouping expr =>
              let val expr = attachBindingToExpr context expr in
                Grouping expr
              end
          | Literal lit => expr
          | Logical (operator, left, right) =>
              let
                val left = attachBindingToExpr context left
                val right = attachBindingToExpr context right
              in
                Logical (operator, left, right)
              end
          | Unary (operator, operand) =>
              let val operand = attachBindingToExpr context operand in
                Unary (operator, operand)
              end
          | Variable (ident, _) =>
              let val binding = Environment.getJumps context ident in
                Variable (ident, SOME binding)
              end
      in
        {value = expr, location = location}
      end

    fun attachBindings initialEnvironment statements =
      let
        val context =
          Environment.make
            (map (fn ident => (ident, nonsenseValue)) initialEnvironment)
      in
        Common.Success (attachBindings' context statements)
      end
  end
