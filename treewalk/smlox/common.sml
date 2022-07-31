structure Common :> COMMON =
  struct
    datatype ('a, 'b) result = Success of 'a | Failure of 'b

    type source_position = { line : int, offset : int }

    type error = { description : string, source_position : source_position }

    fun render_error source { description, source_position } =
    let
      val { line, offset } = source_position
      val preamble = "SMLox encountered an error: " ^ description ^ "\n\n"
    in
      preamble
    end

  end
