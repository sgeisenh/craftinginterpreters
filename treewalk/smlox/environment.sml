structure Environment :> ENVIRONMENT =
  struct
    type t = (string, LoxValue.t) HashTable.hash_table list

    exception UnknownVariable of string

    fun makeInner () =
      HashTable.mkTable
        (HashString.hashString, fn (left, right) => left = right)
        (256, Fail "Unknown variable.")
    fun make () = [makeInner ()]
    fun makeNested context = makeInner () :: context
    fun declare context value = HashTable.insert (hd context) value
    fun get [] ident = raise UnknownVariable ident
      | get (curr :: rest) ident =
        case HashTable.find curr ident of
          SOME v => v
        | NONE => get rest ident
    fun getEnclosing [] _ = NONE
      | getEnclosing (curr :: rest) ident =
        if HashTable.inDomain curr ident then
          SOME curr
        else
          getEnclosing rest ident
    fun assign context (ident, value) =
      case getEnclosing context ident of
        NONE => raise Fail "Unknown variable."
      | SOME scope => HashTable.insert scope (ident, value)
  end
