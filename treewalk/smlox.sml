datatype ('a, 'b) Result = Success of 'a | Failure of 'b

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

    type t = {tokenType : tokenType, lexeme : string, line : int}

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
      | Identifier s => "Identifier(" ^ s ^ ")"
      | String s => "String(" ^ s ^ ")"
      | Number r => "Number(" ^ Real.toString r ^ ")"
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

    fun toString {tokenType, lexeme, line} =
      typeToString tokenType
      ^ " "
      ^ lexeme
      ^ " on line "
      ^ Int.toString line
  end

type scanner =
  { source : string
  , tokens : Token.t list
  , errors : string list
  , start : int
  , current : int
  , line : int
  }

fun isAtEnd {source, current, ...} = current >= String.size source

fun makeScanner program =
  {source = program, tokens = [], errors = [], start = 0, current = 0, line = 1}

fun scanTokens scanner =
  let
    fun createResult {tokens, errors, ...} =
      case errors of
        [] => Success (List.rev tokens)
      | _ => Failure (List.rev errors)
  in
    if isAtEnd scanner then
      createResult scanner
    else
      let
        val {source, tokens, errors, start, current, line} = scanner
        fun addToken tokenType = scanTokens { source = source, tokens = {
          tokenType = tokenType, lexeme = String.substring (source, start,
          current - start + 1), line = line } :: tokens, errors = errors, start =
          current + 1, current = current + 1, line = line }
        fun error msg = scanTokens { source = source, tokens = tokens, errors =
          msg :: errors, start =
          current + 1, current = current + 1, line = line }
        val c = String.sub (source, current)
        fun match 
      in
          case c of
            #"(" => addToken(Token.LeftParen)
          | #")" => addToken(Token.RightParen)
          | #"{" => addToken(Token.LeftBrace)
          | #"}" => addToken(Token.RightBrace)
          | #"," => addToken(Token.Comma)
          | #"." => addToken(Token.Dot)
          | #"-" => addToken(Token.Minus)
          | #"+" => addToken(Token.Plus)
          | #";" => addToken(Token.Semicolon)
          | #"*" => addToken(Token.Star)
          | #"!" => if current + 1 < String.size source and 
          | _ => error "Unexpected character."
      end
  end

fun run program =
  let
    val scanner = makeScanner program
    val tokensOrErrors = scanTokens scanner
  in
    case tokensOrErrors of
      Success tokens =>
        app (fn token => print (Token.toString token ^ "\n")) tokens
    | Failure failures => app (fn failure => print (failure ^ "\n")) failures
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
