structure Binding :> BINDING =
  struct
    open Parser
    val nonsenseValue = LoxValue.Nil

    val peek = hd

    fun isGlobal [_] = true
      | isGlobal _ = false

    fun getFrom [] ident _ = raise Fail "Not that many levels"
      | getFrom (curr :: _) ident 0 = HashTable.inDomain curr
      | getFrom (curr :: rest) ident n = getFrom rest ident (n - 1)

    fun declare context ident =
      let
        val curr = peek context
        val global = isGlobal context
        fun alreadyDeclared () = HashTable.inDomain curr ident
      in
        if not global andalso alreadyDeclared () then
          raise
            Fail ("Redeclaring variable " ^ ident ^ " outside global scope.")
        else
          HashTable.insert curr (ident, false)
      end

    fun define context ident = HashTable.insert (peek context) (ident, true)

    fun makeInner () =
      HashTable.mkTable
        (HashString.hashString, fn (left, right) => left = right)
        (256, Fail "Unknown variable.")

    fun make globals =
      let val context = makeInner () in
        app (HashTable.insert context) (map (fn x => (x, true)) globals);
        [context]
      end

    fun makeNested context = makeInner () :: context

    fun getJumps' [] ident level = raise Fail "Unknown variable."
      | getJumps' (curr :: rest) ident level =
        case HashTable.find curr ident of
          SOME defined =>
            if defined then
              level
            else
              raise Fail "Can't refer to a variable within its own declaration."
        | NONE => getJumps' rest ident (level + 1)
    fun getJumps context ident = getJumps' context ident 0

    fun attachBindings' context = map (attachBinding context)
    and attachBinding context (statement : statement Common.annotated) =
      let
        val {value = statement, location} = statement
        val statement =
          case statement of
            Class (name, _) =>
              (declare context name; define context name; statement)
          | Block statements =>
              let
                val context = makeNested context
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
                val () = define context ident
                val context = makeNested context
                val () =
                  List.app
                    (fn param => (declare context param; define context param))
                    params
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
          | Var (ident, expr) =>
              let
                val () = declare context ident
                val expr = attachBindingToExpr context expr
                val () = define context ident
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
                val binding = getJumps context ident
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
              let val binding = getJumps context ident in
                Variable (ident, SOME binding)
              end
          | Get (expr, name) => Get (attachBindingToExpr context expr, name)
          | Set (expr, ident, value) =>
              let
                val expr = attachBindingToExpr context expr
                val value = attachBindingToExpr context value
              in
                Set (expr, ident, value)
              end
      in
        {value = expr, location = location}
      end

    fun attachBindings initialEnvironment statements =
      let val context = make initialEnvironment in
        Common.Success (attachBindings' context statements)
      end
  end
