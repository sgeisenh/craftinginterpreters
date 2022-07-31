structure Common :> COMMON =
  struct
    datatype ('a, 'b) result = Success of 'a | Failure of 'b

    fun bind f r =
      case r of
        Success x => f x
      | Failure failure => Failure failure

    type source_position = {line : int, offset : int}

    type error = {description : string, source_position : source_position}

    fun repeat (c, n) = String.implode (List.tabulate (n, fn _ => c))

    fun render_error source {description, source_position} =
      let
        val {line, offset} = source_position
        val preamble =
          "SMLox encountered an error: "
          ^ description
          ^ "\nLine: "
          ^ Int.toString line
          ^ " Offset: "
          ^ Int.toString offset
          ^ "\n\n"
        val sourceLines = String.fields (fn c => c = #"\n") source
        val linesAround = 2
        val sourceLines =
          List.drop (sourceLines, Int.max (0, line - linesAround - 1))
            handle Subscript => sourceLines
        val sourceLines =
          List.take (sourceLines, 2 * linesAround + 1)
            handle Subscript => sourceLines
        val indentation = 2
        val sourceLines =
          map (fn line => repeat (#" ", indentation) ^ line ^ "\n") sourceLines
        val linesIncluding =
          List.take (sourceLines, linesAround + 1)
            handle Subscript => sourceLines
        val linesAfter =
          List.drop (sourceLines, linesAround + 1) handle Subscript => []
        val arrowLine = repeat (#" ", offset + indentation - 1) ^ "^"
      in
        String.concat
          [ preamble
          , String.concat linesIncluding
          , arrowLine
          , String.concat linesAfter
          ]
      end
    fun print_errors source errors =
      let
        val messages = map (render_error source) errors
        val fullOutput = String.concatWith "\n" messages ^ "\n"
      in
        print fullOutput
      end
  end
