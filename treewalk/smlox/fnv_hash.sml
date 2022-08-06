structure FNVHash :
  sig

    val offsetBasis : word

    val hashChar : char * word -> word
    val hashString : string -> word

  end =
  struct

    (* values from https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function *)
    val offsetBasis : word = 0wx811c9dc5
    val prime : word = 0wx01000193

    fun hashOne (b, h) = Word.xorb (b, h) * prime

    fun hashChar (c, h) = hashOne (Word.fromInt (Char.ord c), h)

    fun hashString s = CharVector.foldl hashChar offsetBasis s
  end
