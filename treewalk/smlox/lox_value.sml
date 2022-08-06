structure LoxValue :> LOX_VALUE =
  struct
    datatype t =
      Nil
    | Boolean of bool
    | Number of real
    | String of string
    | Function of t list -> t
    | Class of string * t StringTable.hash_table
    | Instance of (string * t StringTable.hash_table) * t StringTable.hash_table

    exception RuntimeError of string

    fun create_instance cls =
      let
        val fields =
          StringTable.mkTable (256, RuntimeError "Undefined property")
      in
        Instance (cls, fields)
      end

    fun eq (Nil, Nil) = Boolean (true)
      | eq (Boolean left, Boolean right) = Boolean (left = right)
      | eq (Number left, Number right) = Boolean (Real.== (left, right))
      | eq (String left, String right) = Boolean (left = right)
      | eq _ = Boolean (false)

    fun neq (left, right) =
      case eq (left, right) of
        Boolean (value) => Boolean (not value)
      | _ => raise RuntimeError "Equality check produced a non-boolean value"

    fun minus (Number left, Number right) = Number (left - right)
      | minus _ = raise RuntimeError "Operands to - must be numbers"

    fun plus (Number left, Number right) = Number (left + right)
      | plus (String left, String right) = String (left ^ right)
      | plus _ =
        raise RuntimeError "Operands to + must both be numbers or strings"

    fun times (Number left, Number right) = Number (left * right)
      | times _ = raise RuntimeError "Operands to * must be numbers"

    fun divides (Number left, Number right) = Number (left / right)
      | divides _ = raise RuntimeError "Operands to / must be numbers"

    fun greater (Number left, Number right) = Boolean (left > right)
      | greater _ = raise RuntimeError "Operands to > must be numbers"

    fun greaterEq (Number left, Number right) = Boolean (left >= right)
      | greaterEq _ = raise RuntimeError "Operands to >= must be numbers"

    fun less (Number left, Number right) = Boolean (left < right)
      | less _ = raise RuntimeError "Operands to < must be numbers"

    fun lessEq (Number left, Number right) = Boolean (left <= right)
      | lessEq _ = raise RuntimeError "Operands to <= must be numbers"

    fun negate (Number operand) = Number (~ operand)
      | negate _ = raise RuntimeError "Operand to unary - must be a number"

    fun logicalNot (Boolean operand) = Boolean (not operand)
      | logicalNot _ = raise RuntimeError "Operand to unary ! must be a boolean"

    fun call (callee, arguments) =
      case callee of
        Function function => function arguments
      | Class cls => create_instance cls
      | _ => raise RuntimeError "can only call functions."

    fun get obj property =
      case obj of
        Instance (cls, fields) => StringTable.lookup fields property
      | _ => raise RuntimeError "Only instances have properties"

    fun set obj ident value =
      case obj of
        Instance (cls, fields) => StringTable.insert fields (ident, value)
      | _ => raise RuntimeError "Only instances have fields"

    fun isTruthy (Boolean false) = false
      | isTruthy Nil = false
      | isTruthy _ = true

    fun toString value =
      case value of
        Nil => "nil"
      | Boolean true => "true"
      | Boolean false => "false"
      | Number r =>
          (let
             val fl = floor r
             val frac = r - real fl
           in
             if Real.== (frac, 0.0) then Int.toString fl else Real.toString r
           end
             handle Overflow => Real.toString r)
      | String s => s
      | Function function => "<function>"
      | Class (name, ctx) => name
      | Instance ((cls, ctx), _) => cls ^ " instance"
  end
