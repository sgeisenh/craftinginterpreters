structure Common :> COMMON =
  struct
    datatype ('a, 'b) result = Success of 'a | Failure of 'b

    fun bind f r =
      case r of
        Success x => f x
      | Failure failure => Failure failure

    type source_position = {line : int, offset : int}
    type source_range = {start : source_position, finish : source_position}
    datatype source_location =
    Position of source_position | Range of source_range

    type 'a annotated = { value: 'a, location: source_location }

    type error = {description : string, source_location : source_location}

    fun repeat (c, n) = String.implode (List.tabulate (n, fn _ => c))

    fun merge_location (left, right) =
      case left of
           Position {lline, loffset} => (case right of
                                              Position {rline, roffset} =>
                                              if rline < lline then
                                                Range { start=right,
                                                finish=left}
                                              else if lline < rline then
                                                Range { start = left,
                                                finish = right}
                                              else if roffset < loffset then
                                                Range { start = right,
                                                finish = left}
                                              else
                                                Range { start = left,
                                                finish = right}
                                            | Range { start=rstart,
                                            finish=rfinish } => 


    fun merge_locations locations =
      List.foldl 

    fun render_position_context source position =
      let
        val {line, offset} = position
        val preamble =
          "Line: "
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

    fun render_error source {description, source_location} =
      let
        val context =
          case source_location of
            Position position => render_position_context source position
          | Range range => "Not implemented yet."
      (* TODO *)
      in
        description ^ "\n" ^ context
      end

    fun print_errors source errors =
      let
        val messages = map (render_error source) errors
        val fullOutput = String.concatWith "\n" messages ^ "\n"
      in
        print fullOutput
      end
  end
