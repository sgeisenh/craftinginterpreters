structure LoxValue :> LOX_VALUE =
  struct
    datatype t =
      Nil
    | Boolean of bool
    | Number of real
    | String of string
    | Fun of t list -> t

    exception RuntimeError of string

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

    fun arity (Callable {arity, ...}) = arity
      | arity _ = raise RuntimeError "arity only available on callables"

    fun call context (callee, arguments) =
      let
        val expected = arity callee
        val actual = List.length arguments
      in
        if expected <> actual then
          raise
            Fail
              ("Expected "
               ^ Int.toString expected
               ^ " arguments but got "
               ^ Int.toString actual
               ^ ".")
        else
          case callee of
            Callable {call, ...} => call (context, arguments)
          | _ => raise RuntimeError "can only call callables."
      end

    fun isTruthy (Boolean false) = false
      | isTruthy Nil = false
      | isTruthy _ = true

    fun toString value =
      case value of
        Nil => "nil"
      | Boolean true => "true"
      | Boolean false => "false"
      | Number r => Real.toString r
      | String s => "\"" ^ s ^ "\""
  end
