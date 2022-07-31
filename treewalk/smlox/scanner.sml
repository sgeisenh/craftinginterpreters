structure Scanner :> SCANNER =
  struct
    open Common

    datatype token =
      LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Identifier of string
    | String of string
    | Number of real
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    | Eof

    fun tokenEqual (left, right) =
      case left of
        LeftParen =>
          (case right of
             LeftParen => true
           | _ => false)
      | RightParen =>
          (case right of
             RightParen => true
           | _ => false)
      | LeftBrace =>
          (case right of
             LeftBrace => true
           | _ => false)
      | RightBrace =>
          (case right of
             RightBrace => true
           | _ => false)
      | Comma =>
          (case right of
             Comma => true
           | _ => false)
      | Dot =>
          (case right of
             Dot => true
           | _ => false)
      | Minus =>
          (case right of
             Minus => true
           | _ => false)
      | Plus =>
          (case right of
             Plus => true
           | _ => false)
      | Semicolon =>
          (case right of
             Semicolon => true
           | _ => false)
      | Slash =>
          (case right of
             Slash => true
           | _ => false)
      | Star =>
          (case right of
             Star => true
           | _ => false)
      | Bang =>
          (case right of
             Bang => true
           | _ => false)
      | BangEqual =>
          (case right of
             BangEqual => true
           | _ => false)
      | Equal =>
          (case right of
             Equal => true
           | _ => false)
      | EqualEqual =>
          (case right of
             EqualEqual => true
           | _ => false)
      | Greater =>
          (case right of
             Greater => true
           | _ => false)
      | GreaterEqual =>
          (case right of
             GreaterEqual => true
           | _ => false)
      | Less =>
          (case right of
             Less => true
           | _ => false)
      | LessEqual =>
          (case right of
             LessEqual => true
           | _ => false)
      | Identifier s =>
          (case right of
             Identifier s' => s = s'
           | _ => false)
      | String s =>
          (case right of
             String s' => s = s'
           | _ => false)
      | Number r =>
          (case right of
             Number r' => Real.== (r, r')
           | _ => false)
      | And =>
          (case right of
             And => true
           | _ => false)
      | Class =>
          (case right of
             Class => true
           | _ => false)
      | Else =>
          (case right of
             Else => true
           | _ => false)
      | False =>
          (case right of
             False => true
           | _ => false)
      | Fun =>
          (case right of
             Fun => true
           | _ => false)
      | For =>
          (case right of
             For => true
           | _ => false)
      | If =>
          (case right of
             If => true
           | _ => false)
      | Nil =>
          (case right of
             Nil => true
           | _ => false)
      | Or =>
          (case right of
             Or => true
           | _ => false)
      | Print =>
          (case right of
             Print => true
           | _ => false)
      | Return =>
          (case right of
             Return => true
           | _ => false)
      | Super =>
          (case right of
             Super => true
           | _ => false)
      | This =>
          (case right of
             This => true
           | _ => false)
      | True =>
          (case right of
             True => true
           | _ => false)
      | Var =>
          (case right of
             Var => true
           | _ => false)
      | While =>
          (case right of
             While => true
           | _ => false)
      | Eof =>
          (case right of
             Eof => true
           | _ => false)

    fun tokenToString token =
      case token of
        LeftParen => "LeftParen"
      | RightParen => "RightParen"
      | LeftBrace => "LeftBrace"
      | RightBrace => "RightBrace"
      | Comma => "Comma"
      | Dot => "Dot"
      | Minus => "Minus"
      | Plus => "Plus"
      | Semicolon => "Semicolon"
      | Slash => "Slash"
      | Star => "Star"
      | Bang => "Bang"
      | BangEqual => "BangEqual"
      | Equal => "Equal"
      | EqualEqual => "EqualEqual"
      | Greater => "Greater"
      | GreaterEqual => "GreaterEqual"
      | Less => "Less"
      | LessEqual => "LessEqual"
      | Identifier s => "Identifier \"" ^ s ^ "\""
      | String s => "String \"" ^ s ^ "\""
      | Number r => "Number " ^ Real.toString r
      | And => "And"
      | Class => "Class"
      | Else => "Else"
      | False => "False"
      | Fun => "Fun"
      | For => "For"
      | If => "If"
      | Nil => "Nil"
      | Or => "Or"
      | Print => "Print"
      | Return => "Return"
      | Super => "Super"
      | This => "This"
      | True => "True"
      | Var => "Var"
      | While => "While"
      | Eof => "Eof"

    type t =
      { source : char list
      , tokens : (token Common.annotated) list
      , errors : Common.error list
      , line : int
      , offset : int
      }

    fun make program =
      { source = String.explode program
      , tokens = []
      , errors = []
      , line = 1
      , offset = 1
      }

    fun dropWhile f [] = []
      | dropWhile f (x :: xs) = if f x then dropWhile f xs else x :: xs

    fun takeWhile f xs =
      let
        fun takeWhile' f [] acc = List.rev acc
          | takeWhile' f (x :: xs) acc =
            if f x then
              takeWhile' f xs (x :: acc)
            else
              List.rev acc
      in
        takeWhile' f xs []
      end

    fun splitWhile f xs =
      let
        fun splitWhile' f [] acc = (List.rev acc, [])
          | splitWhile' f (x :: xs) acc =
            if f x then
              splitWhile' f xs (x :: acc)
            else
              (List.rev acc, x :: xs)
      in
        splitWhile' f xs []
      end

    fun isDigit c = c >= #"0" andalso c <= #"9"

    fun isAlpha c =
      (c >= #"a" andalso c <= #"z") orelse (c >= #"A" andalso c <= #"Z") orelse (c = #"_")

    fun isAlphaNumeric c = isDigit c orelse isAlpha c

    fun terminalEnded [] = true
      | terminalEnded (c :: _) = not (isAlpha c)

    val keywords =
      [ ("and", And)
      , ("class", Class)
      , ("else", Else)
      , ("false", False)
      , ("for", For)
      , ("fun", Fun)
      , ("if", If)
      , ("nil", Nil)
      , ("or", Or)
      , ("print", Print)
      , ("return", Return)
      , ("super", Super)
      , ("this", This)
      , ("true", True)
      , ("var", Var)
      , ("while", While)
      ]

    fun get xs s =
      case List.find (fn (k, v) => k = s) xs of
        NONE => NONE
      | SOME (k, v) => SOME v

    fun scanToken scanner =
      let
        val {source, tokens, errors, line, offset} = scanner
        val start = {line = line, offset = offset}
        fun addToken (token, newSource, offsetDelta) =
          let
            val location =
              Common.Range
                { start = start
                , finish = {line = line, offset = offset + offsetDelta}
                }
          in
            { source = newSource
            , tokens = {location = location, value = token} :: tokens
            , errors = errors
            , line = line
            , offset = offset + offsetDelta
            }
          end
        fun newLine newSource =
          { source = newSource
          , tokens = tokens
          , errors = errors
          , line = line + 1
          , offset = 1
          }
        fun withNewSource (newSource, offset) =
          { source = newSource
          , tokens = tokens
          , errors = errors
          , line = line
          , offset = offset
          }
        fun addString (contents, newSource) =
          let
            val str = String.implode contents
            val strLines = String.fields (fn c => c = #"\n") str
            val newLine =
              line + List.length (List.filter (fn c => c = #"\n") contents)
            val newOffset = 2 + offset + String.size (List.last strLines)
            val location =
              Common.Range
                {start = start, finish = {line = newLine, offset = newOffset}}
          in
            { source = newSource
            , tokens = {location = location, value = String str} :: tokens
            , errors = errors
            , line = newLine
            , offset = newOffset
            }
          end
        fun addIdentifier () =
          let
            val (contents, tl) = splitWhile isAlphaNumeric source
            val identifier = String.implode contents
            val token =
              getOpt ((get keywords identifier), Identifier identifier)
            val newOffset = offset + String.size identifier
            val location =
              Common.Range
                {start = start, finish = {line = line, offset = newOffset}}
          in
            { source = tl
            , tokens = {location = location, value = token} :: tokens
            , errors = errors
            , line = line
            , offset = newOffset
            }
          end
        fun addNumber () =
          let
            val (integer, tl) = splitWhile isDigit source
            val (fractional, tl) =
              case tl of
                #"." :: c :: tl' =>
                  if not (isDigit c) then
                    ([], tl)
                  else
                    let val (fractional, tl') = splitWhile isDigit (c :: tl') in
                      (#"." :: fractional, tl')
                    end
              | _ => ([], tl)
            val numberString =
              String.implode integer ^ String.implode fractional
            val token = Number (valOf (Real.fromString numberString))
            val newOffset = offset + String.size numberString
            val location =
              Common.Range
                {start = start, finish = {line = line, offset = newOffset}}
          in
            { source = tl
            , tokens = {location = location, value = token} :: tokens
            , errors = errors
            , line = line
            , offset = offset + String.size numberString
            }
          end
        fun error (msg, newSource) =
          { source = newSource
          , tokens = tokens
          , errors =
              { description = msg
              , source_location = Common.Position {line = line, offset = offset}
              } :: errors
          , line = line
          , offset = offset
          }
      in
        case source of
          #"(" :: newSource => addToken (LeftParen, newSource, 1)
        | #")" :: newSource => addToken (RightParen, newSource, 1)
        | #"{" :: newSource => addToken (LeftBrace, newSource, 1)
        | #"}" :: newSource => addToken (RightBrace, newSource, 1)
        | #"," :: newSource => addToken (Comma, newSource, 1)
        | #"." :: newSource => addToken (Dot, newSource, 1)
        | #"-" :: newSource => addToken (Minus, newSource, 1)
        | #"+" :: newSource => addToken (Plus, newSource, 1)
        | #";" :: newSource => addToken (Semicolon, newSource, 1)
        | #"*" :: newSource => addToken (Star, newSource, 1)
        | #"/" :: #"/" :: tl =>
            withNewSource (dropWhile (fn c => c <> #"\n") tl, 0)
        | #"/" :: newSource => addToken (Slash, newSource, 1)
        | #"!" :: #"=" :: newSource => addToken (BangEqual, newSource, 2)
        | #"!" :: newSource => addToken (Bang, newSource, 1)
        | #"=" :: #"=" :: newSource => addToken (EqualEqual, newSource, 2)
        | #"=" :: newSource => addToken (Equal, newSource, 1)
        | #"<" :: #"=" :: newSource => addToken (LessEqual, newSource, 2)
        | #"<" :: newSource => addToken (Less, newSource, 1)
        | #">" :: #"=" :: newSource => addToken (GreaterEqual, newSource, 2)
        | #">" :: newSource => addToken (Greater, newSource, 1)
        | #" " :: newSource => withNewSource (newSource, offset + 1)
        | #"\r" :: newSource => withNewSource (newSource, offset + 1)
        | #"\t" :: newSource => withNewSource (newSource, offset + 1)
        | #"\n" :: newSource => newLine newSource
        | #"\"" :: tl =>
            let val (contents, tl) = splitWhile (fn c => c <> #"\"") tl in
              case tl of
                [] => error ("Unterminated string.", [])
              | _ :: tl => addString (contents, tl)
            end
        | c :: tl =>
            if isAlpha c then
              addIdentifier ()
            else if isDigit c then
              addNumber ()
            else
              error ("Unexpected character: " ^ String.str c ^ ".", tl)
        | [] => error ("Empty source.", [])
      end

    fun scanTokens scanner =
      let
        fun createResult {tokens, errors, line, offset, ...} =
          case errors of
            [] =>
              Success
                (List.rev
                   ({ location = Common.Position {line = line, offset = offset}
                    , value = Eof
                    } :: tokens))
          | _ => Failure (List.rev errors)
        val tokens =
          case scanner of
            {source = [], ...} => createResult scanner
          | _ => scanTokens (scanToken scanner)
      in
        tokens
      end
  end
