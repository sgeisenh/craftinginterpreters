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
    Position of source_position | Range of source_range | Unknown

    type 'a annotated = {value : 'a, location : source_location}

    type error = {description : string, source_location : source_location}

    fun repeat (c, n) = String.implode (List.tabulate (n, fn _ => c))

    fun compare_positions (left, right) =
      let
        val {line = lline, offset = loffset} = left
        val {line = rline, offset = roffset} = right
      in
        if lline < rline then
          LESS
        else if lline > rline then
          GREATER
        else if loffset < roffset then
          LESS
        else if loffset > roffset then
          GREATER
        else
          EQUAL
      end

    fun merge_positions (left, right) =
      case compare_positions (left, right) of
        LESS => Range {start = left, finish = right}
      | EQUAL => Position left
      | GREATER => Range {start = right, finish = left}

    fun merge_ranges (left : source_range, right : source_range) =
      let
        val {start = lstart, finish = lfinish} = left
        val {start = rstart, finish = rfinish} = right
        val start =
          case merge_positions (lstart, rstart) of
            Position start => start
          | Range {start, ...} => start
        val finish =
          case merge_positions (lfinish, rfinish) of
            Position finish => finish
          | Range {finish, ...} => finish
      in
        Range {start = start, finish = finish}
      end

    fun merge_range_and_position ( range : source_range
                                 , position : source_position
                                 ) =
      let val position_range = {start = position, finish = position} in
        merge_ranges (range, position_range)
      end

    fun merge_location (left, right) =
      case left of
        Position left =>
          (case right of
             Position right => merge_positions (left, right)
           | Range right => merge_range_and_position (right, left))
      | Range left =>
          (case right of
             Position right => merge_range_and_position (left, right)
           | Range right => merge_ranges (left, right))

    fun merge_locations [] = raise Fail "Can't merge empty locations"
      | merge_locations (first :: rest) = List.foldl merge_location first rest

    val linesAround = 2
    val indentation = 2

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
        val sourceLines =
          List.drop (sourceLines, Int.max (0, line - linesAround - 1))
            handle Subscript => sourceLines
        val sourceLines =
          List.take (sourceLines, 2 * linesAround + 1)
            handle Subscript => sourceLines
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

    fun split' (xs, 0) acc = (List.rev acc, xs)
      | split' ([], n) _ = raise Subscript
      | split' (x :: xs, n) acc = split' (xs, n - 1) (x :: acc)

    fun split (xs, n) = split' (xs, n) []

    fun interleave' ([], []) acc = List.rev acc
      | interleave' (x :: xs, y :: ys) acc =
        interleave' (xs, ys) (y :: x :: acc)
      | interleave' _ _ = raise Fail "Interleaving lists of unequal lengths"

    fun interleave lists = interleave' lists []

    fun createUnderlines ( lines
                         , {line = lline, offset = loffset}
                         , {line = rline, offset = roffset}
                         ) =
      case lines of
        [] => []
      | [line] =>
          [ repeat (#" ", loffset + indentation - 1) ^ repeat
                                                         ( #"~"
                                                         , roffset - loffset
                                                         )
          ]
      | first :: rest =>
          let
            val last = List.last rest
            val middle = List.take (rest, rline - lline - 1)
            val firstLines =
              [ repeat (#" ", loffset + indentation - 1)
                ^ repeat (#"~", String.size first - loffset + 1)
                ^ "\n"
              ]
            val lastLines =
              [repeat (#" ", indentation) ^ repeat (#"~", roffset - 1) ^ "\n"]
            val middleLines =
              map
                (fn line =>
                   repeat (#" ", indentation)
                   ^ repeat (#"~", String.size line)
                   ^ "\n")
                middle
          in
            firstLines @ middleLines @ lastLines
          end

    fun render_range_context source {start, finish} =
      let
        val {line = lline, offset = loffset} = start
        val {line = rline, offset = roffset} = finish
        val preamble =
          "Start: "
          ^ Int.toString lline
          ^ ","
          ^ Int.toString loffset
          ^ " End: "
          ^ Int.toString rline
          ^ ","
          ^ Int.toString roffset
          ^ "\n\n"
        val linesWithin = rline - lline + 1
        val sourceLines = String.fields (fn c => c = #"\n") source
        val preL = Int.max (0, lline - linesAround)
        val preLen = Int.min (preL + linesAround, lline) - preL
        val relevant = List.drop (sourceLines, preL)
        val relevant =
          map (fn line => repeat (#" ", indentation) ^ line ^ "\n") relevant
        val (prefixLines, rest) = split (relevant, preLen)
        val (bodyLines, rest) = split (rest, linesWithin)
        val suffixLines = List.take (rest, linesAround) handle Subscript => rest
        val underlines = createUnderlines (bodyLines, start, finish)
        val resultLines =
          prefixLines @ interleave (bodyLines, underlines) @ suffixLines
      in
        preamble ^ String.concat resultLines
      end

    fun render_error source {description, source_location} =
      let
        val context =
          case source_location of
            Position position => render_position_context source position
          | Range range => render_range_context source range
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
