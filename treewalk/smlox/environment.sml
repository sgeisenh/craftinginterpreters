structure Environment :> ENVIRONMENT =
  struct
    type t = LoxValue.t StringTable.hash_table list

    exception UnknownVariable of string

    fun makeInner () = StringTable.mkTable (256, Fail "Unknown variable.")
    fun make globals =
      let val environment = makeInner () in
        app (StringTable.insert environment) globals; [environment]
      end
    fun makeNested context = makeInner () :: context
    fun declare context value = StringTable.insert (hd context) value
    fun remove context value = ignore (StringTable.remove (hd context) value)
    fun get [] ident = raise UnknownVariable ident
      | get (curr :: rest) ident =
        case StringTable.find curr ident of
          SOME v => v
        | NONE => get rest ident
    fun getEnclosing [] _ = NONE
      | getEnclosing (curr :: rest) ident =
        if StringTable.inDomain curr ident then
          SOME curr
        else
          getEnclosing rest ident
    fun assign context (ident, value) =
      case getEnclosing context ident of
        NONE => raise Fail "Unknown variable."
      | SOME scope => StringTable.insert scope (ident, value)
    fun isGlobal [_] = true
      | isGlobal _ = false

    fun getJumps' [] ident level = raise UnknownVariable ident
      | getJumps' (curr :: rest) ident level =
        case StringTable.find curr ident of
          SOME _ => level
        | NONE => getJumps' rest ident (level + 1)
    fun getJumps context ident = getJumps' context ident 0

    fun getFrom [] _ _ = raise Fail "Unreachable"
      | getFrom (curr :: rest) ident 0 = StringTable.lookup curr ident
      | getFrom (curr :: rest) ident level = getFrom rest ident (level - 1)

    fun assignTo [] _ _ = raise Fail "Unreachable"
      | assignTo context pair 0 = StringTable.insert (hd context) pair
      | assignTo (curr :: rest) pair n = assignTo rest pair (n - 1)
  end
