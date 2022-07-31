signature COMMON = sig
  datatype ('a, 'b) result = Success of 'a | Failure of 'b

  type source_position = { line : int, offset : int }

  type error = { description : string, source_position : source_position }

  val render_error : string -> error -> string
end
