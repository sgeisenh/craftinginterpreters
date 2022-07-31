signature COMMON =
  sig
    datatype ('a, 'b) result = Success of 'a | Failure of 'b
    val bind : ('a -> ('b, 'c) result) -> ('a, 'c) result -> ('b, 'c) result

    type source_position = {line : int, offset : int}
    type source_range = {start : source_position, finish : source_position}
    datatype source_location =
    Position of source_position | Range of source_range | Unknown
    type 'a annotated = {value : 'a, location : source_location}
    val merge_locations : source_location list -> source_location

    type error = {description : string, source_location : source_location}

    val print_errors : string -> error list -> unit
  end
