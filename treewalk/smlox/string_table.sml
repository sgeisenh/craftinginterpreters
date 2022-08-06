structure StringTable =
  HashTableFn
    (struct
        type hash_key = string
       val hashVal = FNVHash.hashString
       fun sameKey (left, right) = left = right
     end)
