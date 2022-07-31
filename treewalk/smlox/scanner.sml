structure Scanner :> SCANNER =
  struct
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

    type error = {message : string, line : int}
    type t =
      {source : char list, tokens : token list, errors : error list, line : int}

    fun make program =
      {source = String.explode program, tokens = [], errors = [], line = 1}

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
        val {source, tokens, errors, line} = scanner
        fun addToken (token, newSource) =
          { source = newSource
          , tokens = token :: tokens
          , errors = errors
          , line = line
          }
        fun newLines (newSource, numLines) =
          { source = newSource
          , tokens = tokens
          , errors = errors
          , line = line + numLines
          }
        fun withNewSource newSource =
          {source = newSource, tokens = tokens, errors = errors, line = line}
        fun addString (contents, newSource) =
          { source = newSource
          , tokens =
              String (String.implode contents) :: tokens
          , errors = errors
          , line = line + List.length (List.filter (fn c => c = #"\n") contents)
          }
        fun addIdentifier () =
          let
            val (contents, tl) = splitWhile isAlphaNumeric source
            val identifier = String.implode contents
            val token =
              getOpt ((get keywords identifier), Identifier identifier)
          in
            { source = tl
            , tokens = token :: tokens
            , errors = errors
            , line = line
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
            val number =
              Number
                (valOf
                   (Real.fromString
                      (String.implode integer ^ String.implode fractional)))
          in
            { source = tl
            , tokens = number :: tokens
            , errors = errors
            , line = line
            }
          end
        fun error (msg, newSource) =
          { source = newSource
          , tokens = tokens
          , errors = {message = msg, line = line} :: errors
          , line = line
          }
      in
        case source of
          #"(" :: newSource => addToken (LeftParen, newSource)
        | #")" :: newSource => addToken (RightParen, newSource)
        | #"{" :: newSource => addToken (LeftBrace, newSource)
        | #"}" :: newSource => addToken (RightBrace, newSource)
        | #"," :: newSource => addToken (Comma, newSource)
        | #"." :: newSource => addToken (Dot, newSource)
        | #"-" :: newSource => addToken (Minus, newSource)
        | #"+" :: newSource => addToken (Plus, newSource)
        | #";" :: newSource => addToken (Semicolon, newSource)
        | #"*" :: newSource => addToken (Star, newSource)
        | #"/" :: #"/" :: tl => withNewSource (dropWhile (fn c => c <> #"\n") tl)
        | #"/" :: newSource => addToken (Slash, newSource)
        | #"!" :: #"=" :: newSource => addToken (BangEqual, newSource)
        | #"!" :: newSource => addToken (Bang, newSource)
        | #"=" :: #"=" :: newSource => addToken (EqualEqual, newSource)
        | #"=" :: newSource => addToken (Equal, newSource)
        | #"<" :: #"=" :: newSource => addToken (LessEqual, newSource)
        | #"<" :: newSource => addToken (Less, newSource)
        | #">" :: #"=" :: newSource => addToken (GreaterEqual, newSource)
        | #">" :: newSource => addToken (Greater, newSource)
        | #" " :: newSource => withNewSource newSource
        | #"\r" :: newSource => withNewSource newSource
        | #"\t" :: newSource => withNewSource newSource
        | #"\n" :: newSource => newLines (newSource, 1)
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
              error ("Unexpected character.", tl)
        | [] => error ("Empty source.", [])
      end

    fun scanTokens scanner =
      let
        fun createResult {source, tokens, errors, line} =
          case errors of
            [] =>
              Result.Success (List.rev (Eof :: tokens))
          | _ => Result.Failure (List.rev errors)
      in
        case scanner of
          {source = [], ...} => createResult scanner
        | _ => scanTokens (scanToken scanner)
      end
  end
