structure Binding :> BINDING =
struct
  val nonsenseValue = LoxValue.Nil

  fun attachBindings' context = map (attachBinding context)
  fun attachBinding context statement =
    case statement of
         Block statements => 
       | Expression expr => raise Fail ""
       | Function (ident, params, body) => raise Fail ""
       | If (cond, thn, els) => raise Fail ""
       | While (cond, body) => raise Fail ""
       | Print expr => raise Fail ""
       | Return expr => raise Fail ""
       | Var (ident, expr) => raise Fail ""

  fun attachBindings initialEnvironment statements =
  let
    val context = Environment.make (map (fn ident => (ident, nonsenseValue))
    initialEnvironment)
  in
    attachBindings' context statements
  end
end
