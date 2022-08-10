structure Resolver :> RESOLVER =
  struct
    open Parser

    datatype function_kind = NoFunction | FunctionKind | MethodKind | InitKind
    datatype class_type = NoClass | ClassType

    exception UnknownVariable

    val peek = hd

    fun isGlobal [_] = true
      | isGlobal _ = false

    fun getFrom [] ident _ = raise Fail "Not that many levels"
      | getFrom (curr :: _) ident 0 = StringTable.inDomain curr
      | getFrom (curr :: rest) ident n = getFrom rest ident (n - 1)

    fun declare context ident =
      let
        val curr = peek context
        val global = isGlobal context
        fun alreadyDeclared () = StringTable.inDomain curr ident
      in
        if not global andalso alreadyDeclared () then
          raise
            Fail ("Redeclaring variable " ^ ident ^ " outside global scope.")
        else
          StringTable.insert curr (ident, false)
      end

    fun define context ident = StringTable.insert (peek context) (ident, true)

    fun makeInner () = StringTable.mkTable (256, Fail "Unknown variable.")

    fun make globals =
      let val context = makeInner () in
        app (StringTable.insert context) (map (fn x => (x, true)) globals);
        [context]
      end

    fun makeNested context = makeInner () :: context

    fun getJumps' [] ident level = raise UnknownVariable
      | getJumps' (curr :: rest) ident level =
        case StringTable.find curr ident of
          SOME defined =>
            if defined then
              level
            else
              raise Fail "Can't refer to a variable within its own declaration."
        | NONE => getJumps' rest ident (level + 1)
    fun getJumps context ident = getJumps' context ident 0

    fun attachBindings' context currentFunction currentClass =
      map (attachBinding context currentFunction currentClass)
    and attachBinding context currentFunction currentClass statement =
      let
        val {value = statement, location} = statement
        val statement =
          case statement of
            Class (name, superclass, methods) =>
              let
                val () = (declare context name; define context name)
                val superclass =
                  case superclass of
                    NONE => NONE
                  | SOME superclass =>
                      let
                        val () =
                          case superclass of
                            {value = Variable (superclass, _), ...} =>
                              if superclass = name then
                                raise Fail "A class can't inherit from itself."
                              else
                                ()
                          | _ => raise Fail "Unreachable"
                      in
                        SOME
                          (attachBindingToExpr context currentClass superclass)
                      end
                val context = makeNested context
                val () =
                  case superclass of
                    NONE => ()
                  | SOME _ => (declare context "super"; define context "super")
                val () = (declare context "this"; define context "this")
                val methods =
                  attachBindings' context NoFunction ClassType methods
              in
                Class (name, superclass, methods)
              end
          | Block statements =>
              let
                val context = makeNested context
                val statements =
                  attachBindings' context currentFunction currentClass
                    statements
              in
                Block statements
              end
          | Expression expr =>
              let val expr = attachBindingToExpr context currentClass expr in
                Expression expr
              end
          | Function (ident, params, body, kind) =>
              let
                val () = declare context ident
                val () = define context ident
                val context = makeNested context
                val () =
                  List.app
                    (fn param => (declare context param; define context param))
                    params
                val currentFunction =
                  case kind of
                    Func => FunctionKind
                  | Method => if ident = "init" then InitKind else MethodKind
                val body =
                  attachBindings' context currentFunction currentClass body
              in
                Function (ident, params, body, kind)
              end
          | If (cond, thn, els) =>
              let
                val cond = attachBindingToExpr context currentClass cond
                val thn = attachBinding context currentFunction currentClass thn
                val els =
                  case els of
                    NONE => els
                  | SOME els =>
                      SOME
                        (attachBinding context currentFunction currentClass els)
              in
                If (cond, thn, els)
              end
          | While (cond, body) =>
              let
                val cond = attachBindingToExpr context currentClass cond
                val body =
                  attachBinding context currentFunction currentClass body
              in
                While (cond, body)
              end
          | Print expr =>
              let val expr = attachBindingToExpr context currentClass expr in
                Print expr
              end
          | Return expr =>
              (case currentFunction of
                 NoFunction => raise Fail "Can't return outside of a function"
               | InitKind =>
                   (case expr of
                      NONE => Return expr
                    | _ => raise Fail "Can't return a value from an initializer")
               | _ =>
                   let
                     val expr =
                       case expr of
                         NONE => NONE
                       | SOME expr =>
                           SOME (attachBindingToExpr context currentClass expr)
                   in
                     Return expr
                   end)
          | Var (ident, expr) =>
              let
                val () = declare context ident
                val expr = attachBindingToExpr context currentClass expr
                val () = define context ident
              in
                Var (ident, expr)
              end
      in
        {value = statement, location = location}
      end
    and attachBindingToExpr context currentClass expr =
      let
        val {value = expr, location} = expr
        val recurse = attachBindingToExpr context currentClass
        val expr =
          case expr of
            Assign ((ident, _), expr) =>
              let
                val expr = recurse expr
                val binding = getJumps context ident
              in
                Assign ((ident, SOME binding), expr)
              end
          | Binary (operator, left, right) =>
              let
                val left = recurse left
                val right = recurse right
              in
                Binary (operator, left, right)
              end
          | Call (callee, args) =>
              let
                val callee = recurse callee
                val args = map recurse args
              in
                Call (callee, args)
              end
          | Grouping expr => let val expr = recurse expr in Grouping expr end
          | Literal lit => expr
          | Logical (operator, left, right) =>
              let
                val left = recurse left
                val right = recurse right
              in
                Logical (operator, left, right)
              end
          | Unary (operator, operand) =>
              let val operand = recurse operand in Unary (operator, operand) end
          | Variable (ident, _) =>
              let
                val binding =
                  SOME (getJumps context ident) handle UnknownVariable => NONE
              in
                Variable (ident, binding)
              end
          | Get (expr, name) => Get (recurse expr, name)
          | Set (expr, ident, value) =>
              let
                val expr = recurse expr
                val value = recurse value
              in
                Set (expr, ident, value)
              end
          | Super method => (getJumps context "super"; Super method)
          | This _ =>
              case currentClass of
                NoClass => raise Fail "Can't use 'this' outside of a class."
              | _ =>
                  let val binding = getJumps context "this" in
                    This (SOME binding)
                  end
      in
        {value = expr, location = location}
      end

    fun attachBindings initialEnvironment statements =
      let val context = make initialEnvironment in
        Common.Success (attachBindings' context NoFunction NoClass statements)
      end
  end
