structure LoxValue :> LOX_VALUE =
  struct
    datatype t =
      Nil
    | Boolean of bool
    | Number of real
    | String of string
    | Function of string * (t list -> t)
    | Class of class
    | Instance of class * t StringTable.hash_table
    and class =
    ClassType of string * (t -> t) StringTable.hash_table * class option

    exception RuntimeError of string
    exception ReturnExn of t

    fun createInstance cls =
      let
        val fields =
          StringTable.mkTable (256, RuntimeError "Undefined property")
      in
        Instance (cls, fields)
      end

    fun findMethod (ClassType (_, methods, superclass)) name =
      case StringTable.find methods name of
        SOME method => SOME method
      | NONE =>
          (case superclass of
             SOME superclass => findMethod superclass name
           | NONE => NONE)

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

    fun isTruthy (Boolean false) = false
      | isTruthy Nil = false
      | isTruthy _ = true

    fun logicalNot value = Boolean (not (isTruthy value))

    fun call (callee, arguments) =
      case callee of
        Function (_, function) => function arguments
      | Class (class) =>
          let val instance = createInstance class in
            case findMethod class "init" of
              NONE =>
                (case arguments of
                   [] => instance
                 | _ =>
                     raise
                       RuntimeError "Calling default constructor with arguments")
            | SOME constructor =>
                (call (constructor instance, arguments); instance)
                  handle ReturnExn _ => instance
          end
      | _ => raise RuntimeError "can only call functions."

    fun get obj property =
      case obj of
        Instance (classinfo, fields) =>
          (case StringTable.find fields property of
             SOME prop => prop
           | NONE =>
               (case findMethod classinfo property of
                  SOME method => method obj
                | NONE => raise RuntimeError "No property."))
      | _ => raise RuntimeError "Only instances have properties"

    fun set obj ident value =
      case obj of
        Instance (cls, fields) => StringTable.insert fields (ident, value)
      | _ => raise RuntimeError "Only instances have fields"

    fun toString value =
      case value of
        Nil => "nil"
      | Boolean true => "true"
      | Boolean false => "false"
      | Number r =>
          let val result = Real.toString r in
            String.map (fn #"~" => #"-" | c => c) result
          end
      | String s => s
      | Function (name, _) => "<" ^ name ^ ">"
      | Class (ClassType (name, _, _)) => name
      | Instance (ClassType (cls, _, _), _) => cls ^ " instance"
  end
