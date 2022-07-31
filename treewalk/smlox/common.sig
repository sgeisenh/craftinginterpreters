signature COMMON =
  sig
    datatype ('a, 'b) result = Success of 'a | Failure of 'b
    val bind : ('a -> ('b, 'c) result) -> ('a, 'c) result -> ('b, 'c) result

    type source_position = {line : int, offset : int}

    type error = {description : string, source_position : source_position}

    val print_errors : string -> error list -> unit
  end
