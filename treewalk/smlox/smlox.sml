datatype ('a, 'b) Result = Success of 'a | Failure of 'b

structure Ast =
  struct
    datatype literal = Number of real | String of string | True | False | Nil
    datatype binary_operator =
      Dot
    | Minus
    | Plus
    | Slash
    | Star
    | BangEqual
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | And
    | Or
    datatype unary_operator = Negative | Bang
    datatype expr =
      Binary of (binary_operator * expr * expr)
    | Grouping of expr
    | Literal of literal
    | Unary of (unary_operator * expr)

    fun binOpToString operator =
      case operator of
        Dot => "Dot"
      | Minus => "Minus"
      | Plus => "Plus"
      | Slash => "Slash"
      | Star => "Star"
      | BangEqual => "BangEqual"
      | EqualEqual => "EqualEqual"
      | Greater => "Greater"
      | GreaterEqual => "GreaterEqual"
      | Less => "Less"
      | LessEqual => "LessEqual"
      | And => "And"
      | Or => "Or"
    fun unaryOpToString operator =
      case operator of
        Negative => "Negative"
      | Bang => "Bang"
    fun literalToString literal =
      case literal of
        Number r => Real.toString r
      | String s => "\"" ^ s ^ "\""
      | True => "true"
      | False => "false"
      | Nil => "nil"
    fun exprToString expr =
      case expr of
        Binary (operator, left, right) =>
          "("
          ^ binOpToString operator
          ^ " "
          ^ exprToString left
          ^ " "
          ^ exprToString right
          ^ ")"
      | Grouping expr => "(group" ^ exprToString expr ^ ")"
      | Literal literal => literalToString literal
      | Unary (operator, expr) =>
          "("
          ^ unaryOpToString operator
          ^ " "
          ^ exprToString expr
          ^ ")"
  end

structure Token =
  struct
    datatype tokenType =
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

    type t = {tokenType : tokenType, line : int}

    fun typeToString tokenType =
      case tokenType of
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

    fun toString {tokenType, line} =
      "{ tokenType = "
      ^ typeToString tokenType
      ^ ", line = "
      ^ Int.toString line
      ^ " }"
  end

type error = {message : string, line : int}

type scanner =
  {source : char list, tokens : Token.t list, errors : string list, line : int}

fun makeScanner program =
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
  [ ("and", Token.And)
  , ("class", Token.Class)
  , ("else", Token.Else)
  , ("false", Token.False)
  , ("for", Token.For)
  , ("fun", Token.Fun)
  , ("if", Token.If)
  , ("nil", Token.Nil)
  , ("or", Token.Or)
  , ("print", Token.Print)
  , ("return", Token.Return)
  , ("super", Token.Super)
  , ("this", Token.This)
  , ("true", Token.True)
  , ("var", Token.Var)
  , ("while", Token.While)
  ]

fun get xs s =
  case List.find (fn (k, v) => k = s) xs of
    NONE => NONE
  | SOME (k, v) => SOME v

fun scanToken scanner =
  let
    val {source, tokens, errors, line} = scanner
    fun addToken (tokenType, newSource) =
      { source = newSource
      , tokens = {tokenType = tokenType, line = line} :: tokens
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
          {tokenType = Token.String (String.implode contents), line = line} :: tokens
      , errors = errors
      , line = line + List.length (List.filter (fn c => c = #"\n") contents)
      }
    fun addIdentifier () =
      let
        val (contents, tl) = splitWhile isAlphaNumeric source
        val identifier = String.implode contents
        val tokenType =
          getOpt ((get keywords identifier), Token.Identifier identifier)
      in
        { source = tl
        , tokens = {tokenType = tokenType, line = line} :: tokens
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
          Token.Number
            (valOf
               (Real.fromString
                  (String.implode integer ^ String.implode fractional)))
      in
        { source = tl
        , tokens = {tokenType = number, line = line} :: tokens
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
      #"(" :: newSource => addToken (Token.LeftParen, newSource)
    | #")" :: newSource => addToken (Token.RightParen, newSource)
    | #"{" :: newSource => addToken (Token.LeftBrace, newSource)
    | #"}" :: newSource => addToken (Token.RightBrace, newSource)
    | #"," :: newSource => addToken (Token.Comma, newSource)
    | #"." :: newSource => addToken (Token.Dot, newSource)
    | #"-" :: newSource => addToken (Token.Minus, newSource)
    | #"+" :: newSource => addToken (Token.Plus, newSource)
    | #";" :: newSource => addToken (Token.Semicolon, newSource)
    | #"*" :: newSource => addToken (Token.Star, newSource)
    | #"/" :: #"/" :: tl => withNewSource (dropWhile (fn c => c <> #"\n") tl)
    | #"/" :: newSource => addToken (Token.Slash, newSource)
    | #"!" :: #"=" :: newSource => addToken (Token.BangEqual, newSource)
    | #"!" :: newSource => addToken (Token.Bang, newSource)
    | #"=" :: #"=" :: newSource => addToken (Token.EqualEqual, newSource)
    | #"=" :: newSource => addToken (Token.Equal, newSource)
    | #"<" :: #"=" :: newSource => addToken (Token.LessEqual, newSource)
    | #"<" :: newSource => addToken (Token.Less, newSource)
    | #">" :: #"=" :: newSource => addToken (Token.GreaterEqual, newSource)
    | #">" :: newSource => addToken (Token.Greater, newSource)
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
    fun createResult {tokens, errors, line, ...} =
      case errors of
        [] =>
          Success (List.rev ({tokenType = Token.Eof, line = line} :: tokens))
      | _ => Failure (List.rev errors)
  in
    case scanner of
      {source = [], ...} => createResult scanner
    | _ => scanTokens (scanToken scanner)
  end

fun run program =
  let
    val scanner = makeScanner program
    val tokensOrErrors = scanTokens scanner
  in
    case tokensOrErrors of
      Success tokens =>
        app (fn token => print (Token.toString token ^ "\n")) tokens
    | Failure failures =>
        app
          (fn {message, line} =>
             print
               ("Error on line "
                ^ Int.toString (line)
                ^ ": "
                ^ message
                ^ "\n"))
          failures
  end

fun runFile filename =
  let val program = TextIO.input (TextIO.openIn filename) in run program end

fun runPrompt () =
  while true do
    let val maybeLine = TextIO.inputLine TextIO.stdIn in
      case maybeLine of
        SOME line => run line
      | NONE => OS.Process.exit OS.Process.success
    end

val () =
  case CommandLine.arguments () of
    [] => runPrompt ()
  | s :: [] => runFile s
  | _ => (print "Usage: smlox [script]\n"; OS.Process.exit OS.Process.failure)
