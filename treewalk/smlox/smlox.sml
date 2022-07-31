datatype ('a, 'b) Result = Success of 'a | Failure of 'b

structure LoxValue =
  struct
    datatype t = Nil | Boolean of bool | Number of real | String of string

    fun eq (Nil, Nil) = Boolean (true)
      | eq (Boolean left, Boolean right) = Boolean (left = right)
      | eq (Number left, Number right) = Boolean (Real.== (left, right))
      | eq (String left, String right) = Boolean (left = right)
      | eq _ = Boolean (false)

    fun neq (left, right) =
      case eq (left, right) of
        Boolean (value) => Boolean (not value)
      | _ => raise Fail "Equality check produced a non-boolean value"

    fun minus (Number left, Number right) = Number (left - right)
      | minus _ = raise Fail "Operands to - must be numbers"

    fun plus (Number left, Number right) = Number (left + right)
      | plus (String left, String right) = String (left ^ right)
      | plus _ = raise Fail "Operands to + must both be numbers or strings"

    fun times (Number left, Number right) = Number (left * right)
      | times _ = raise Fail "Operands to * must be numbers"

    fun divides (Number left, Number right) = Number (left / right)
      | divides _ = raise Fail "Operands to / must be numbers"

    fun greater (Number left, Number right) = Boolean (left > right)
      | greater _ = raise Fail "Operands to > must be numbers"

    fun greaterEq (Number left, Number right) = Boolean (left >= right)
      | greaterEq _ = raise Fail "Operands to >= must be numbers"

    fun less (Number left, Number right) = Boolean (left < right)
      | less _ = raise Fail "Operands to < must be numbers"

    fun lessEq (Number left, Number right) = Boolean (left <= right)
      | lessEq _ = raise Fail "Operands to <= must be numbers"

    fun negate (Number operand) = Number (~ operand)
      | negate _ = raise Fail "Operand to unary - must be a number"

    fun logicalNot (Boolean operand) = Boolean (not operand)
      | logicalNot _ = raise Fail "Operand to unary ! must be a boolean"

    fun toString value =
      case value of
        Nil => "nil"
      | Boolean true => "true"
      | Boolean false => "false"
      | Number r => Real.toString r
      | String s => "\"" ^ s ^ "\""
  end

structure Context =
  struct
    type t = (string, LoxValue.t) HashTable.hash_table list

    fun makeInner () =
      HashTable.mkTable
        (HashString.hashString, fn (left, right) => left = right)
        (256, Fail "Unknown variable.")
    fun make () = [makeInner ()]
    fun add context value = HashTable.insert (hd context) value
    fun get [] _ = raise Fail "Unknown variable."
      | get (curr :: rest) ident =
        case HashTable.find curr ident of
          SOME v => v
        | NONE => get rest ident
    fun getEnclosing [] _ = NONE
      | getEnclosing (curr :: rest) ident =
        if HashTable.inDomain curr ident then
          SOME curr
        else
          getEnclosing rest ident
    fun assign context (ident, value) =
      case getEnclosing context ident of
        NONE => raise Fail "Unknown variable."
      | SOME scope => HashTable.insert scope (ident, value)
    fun makeNested context = makeInner () :: context
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

    fun tokEq left right =
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
      Assign of (string * expr)
    | Binary of (binary_operator * expr * expr)
    | Grouping of expr
    | Literal of literal
    | Unary of (unary_operator * expr)
    | Variable of string
    datatype statement =
      Block of statement list
    | Expression of expr
    | Print of expr
    | Var of (string * expr)

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
        Assign (ident, expr) =>
          "(assign "
          ^ ident
          ^ " "
          ^ exprToString expr
          ^ ")"
      | Binary (operator, left, right) =>
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
      | Variable ident => "(variable " ^ ident ^ ")"

    fun matchTypes (types : Token.tokenType list) (tokens : Token.tokenType list) =
      case tokens of
        [] => NONE
      | token :: tokens' =>
          if List.exists (fn typ => Token.tokEq typ token) types then
            SOME (token, tokens')
          else
            NONE

    fun tokenToBinop token =
      case token of
        Token.Minus => Minus
      | Token.Plus => Plus
      | Token.Slash => Slash
      | Token.Star => Star
      | Token.BangEqual => BangEqual
      | Token.EqualEqual => EqualEqual
      | Token.Greater => Greater
      | Token.GreaterEqual => GreaterEqual
      | Token.Less => Less
      | Token.LessEqual => LessEqual
      | Token.And => And
      | Token.Or => Or
      | _ => raise Fail "Unknown binary operator token"

    fun tokenToUnop token =
      case token of
        Token.Minus => Negative
      | Token.Bang => Bang
      | _ => raise Fail "Unknown unary operator token"

    fun parseBinaryLevel types next tokens =
      let val (left, tokens') = next (tokens) in
        case matchTypes types tokens' of
          NONE => (left, tokens')
        | SOME (token, tokens') =>
            let
              val (right, tokens') = parseBinaryLevel types next tokens'
              val operator = tokenToBinop token
            in
              (Binary (operator, left, right), tokens')
            end
      end
     

    fun parse tokens = parseStatements tokens
    and parseStatements tokens = parseStatements' (tokens, [])
    and parseStatements' (tokens, acc) =
      case tokens of
        [] => List.rev acc
      | [Token.Eof] => List.rev acc
      | _ =>
          let val (statement, tokens') = parseStatement tokens in
            parseStatements' (tokens', (statement :: acc))
          end
    and parseStatement tokens =
      case tokens of
        Token.Var :: tokens' => parseVarDeclaration tokens'
      | Token.Print :: tokens' => parsePrintStatement tokens'
      | Token.LeftBrace :: tokens' => parseBlock tokens'
      | _ => parseExpressionStatement tokens
    and parseVarDeclaration tokens =
      case tokens of
        (Token.Identifier ident) :: tokens' =>
          (case matchTypes [Token.Equal] tokens' of
             NONE => (Var (ident, Literal Nil), tokens')
           | SOME (_, tokens') =>
               let
                 val (expr, tokens') = parseExpression tokens'
                 val tokens' =
                   case matchTypes [Token.Semicolon] tokens' of
                     NONE => raise Fail "Expect ';' after variable declaration."
                   | SOME (_, tokens' : Token.tokenType list) => tokens'
               in
                 (Var (ident, expr), tokens')
               end)
      | _ => raise Fail "Expect variable name."
    and parsePrintStatement tokens =
      let
        val (expr : expr, tokens' : Token.tokenType list) =
          parseExpression tokens
        val tokens' =
          case matchTypes [Token.Semicolon] tokens' of
            NONE => raise Fail "Expect ';' after value."
          | SOME (_, tokens') => tokens'
      in
        (Print expr, tokens')
      end
    and parseBlock tokens = parseBlock' tokens []
    and parseBlock' tokens acc =
      case tokens of
        [] => raise Fail "Expect '}' after block."
      | Token.RightBrace :: tokens => (Block (List.rev acc), tokens)
      | _ =>
          let val (statement, tokens) = parseStatement tokens in
            parseBlock' tokens (statement :: acc)
          end
    and parseExpressionStatement tokens =
      let
        val (expr, tokens') = parseExpression tokens
        val tokens' =
          case matchTypes [Token.Semicolon] tokens' of
            NONE => raise Fail "Expect ';' after expression."
          | SOME (_, tokens') => tokens'
      in
        (Expression expr, tokens')
      end
    and parseExpression (tokens : Token.tokenType list) : (expr * Token.tokenType list) =
      parseAssignment tokens
    and parseAssignment tokens =
      let val (left, tokens) = parseEquality tokens in
        case matchTypes [Token.Equal] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let val (value, tokens) = parseAssignment tokens in
              case left of
                Variable ident => (Assign (ident, value), tokens)
              | _ => raise Fail "Expected ident."
            end
      end
    and parseEquality (tokens : Token.tokenType list) =
      parseBinaryLevel [Token.BangEqual, Token.EqualEqual] parseComparison
        tokens
    and parseComparison (tokens : Token.tokenType list) =
      parseBinaryLevel
        [Token.Greater, Token.GreaterEqual, Token.Less, Token.LessEqual]
        parseTerm
        tokens
    and parseTerm (tokens : Token.tokenType list) =
      parseBinaryLevel [Token.Minus, Token.Plus] parseFactor tokens
    and parseFactor (tokens : Token.tokenType list) =
      parseBinaryLevel [Token.Slash, Token.Star] parseUnary tokens
    and parseUnary (tokens : Token.tokenType list) =
      case matchTypes [Token.Bang, Token.Minus] tokens of
        NONE => parsePrimary tokens
      | SOME (token, tokens') =>
          let val (expr, tokens') = parseUnary tokens' in
            (Unary (tokenToUnop token, expr), tokens')
          end
    and parsePrimary (tokens : Token.tokenType list) =
      case tokens of
        [] => raise Fail "Unable to parse primary from empty tokens"
      | token :: tokens' =>
          case token of
            Token.False => (Literal False, tokens')
          | Token.True => (Literal True, tokens')
          | Token.Nil => (Literal Nil, tokens')
          | Token.Number r => (Literal (Number r), tokens')
          | Token.String s => (Literal (String s), tokens')
          | Token.LeftParen =>
              let val (expr, tokens') = parseExpression tokens' in
                case matchTypes [Token.RightParen] tokens' of
                  NONE => raise Fail "Expect ')' after expression."
                | SOME (token, tokens') => (Grouping expr, tokens')
              end
          | Token.Identifier ident => (Variable ident, tokens')
          | _ => raise Fail "Expect expression."

    fun evaluateExpr context expr =
      case expr of
        Assign (ident, expr) =>
          let val result = evaluateExpr context expr in
            Context.assign context (ident, result); result
          end
      | Binary (binOp, left, right) =>
          let
            val left' = evaluateExpr context left
            val right' = evaluateExpr context right
          in
            (case binOp of
               Dot => raise Fail "Unimplemented"
             | Minus => LoxValue.minus (left', right')
             | Plus => LoxValue.plus (left', right')
             | Slash => LoxValue.divides (left', right')
             | Star => LoxValue.times (left', right')
             | BangEqual => LoxValue.neq (left', right')
             | EqualEqual => LoxValue.eq (left', right')
             | Greater => LoxValue.greater (left', right')
             | GreaterEqual => LoxValue.greaterEq (left', right')
             | Less => LoxValue.less (left', right')
             | LessEqual => LoxValue.lessEq (left', right')
             | And => raise Fail "Unimplemented"
             | Or => raise Fail "Unimplemented")
          end
      | Grouping expr' => evaluateExpr context expr'
      | Literal literal =>
          (case literal of
             Number r => LoxValue.Number r
           | String s => LoxValue.String s
           | True => LoxValue.Boolean true
           | False => LoxValue.Boolean false
           | Nil => LoxValue.Nil)
      | Unary (unOp, expr') =>
          let val expr' = evaluateExpr context expr' in
            case unOp of
              Bang => LoxValue.logicalNot expr'
            | Negative => LoxValue.negate expr'
          end
      | Variable ident => Context.get context ident
  end

structure Interpreter =
  struct
    datatype t = unit

    fun evaluateStatement context statement =
      case statement of
        Ast.Block statements =>
          List.app (evaluateStatement (Context.makeNested context)) statements
      | Ast.Expression expr => (Ast.evaluateExpr context expr; ())
      | Ast.Print expr =>
          let val result = Ast.evaluateExpr context expr in
            print (LoxValue.toString result ^ "\n")
          end
      | Ast.Var (ident, expr) =>
          let val result = Ast.evaluateExpr context expr in
            Context.add context (ident, result)
          end

    fun interpret context = List.app (evaluateStatement context)
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
    fun createResult {source, tokens, errors, line} =
      case errors of
        [] =>
          Success (List.rev ({tokenType = Token.Eof, line = line} :: tokens))
      | _ => Failure (List.rev errors)
  in
    case scanner of
      {source = [], ...} => createResult scanner
    | _ => scanTokens (scanToken scanner)
  end

fun run context program =
  let
    val scanner = makeScanner program
    val tokensOrErrors = scanTokens scanner
    val justTokens =
      case tokensOrErrors of
        Success tokens => map (fn {tokenType, ...} => tokenType) tokens
      | Failure failures =>
          ( (app
               (fn {message, line} =>
                  print
                    ("Error on line "
                     ^ Int.toString line
                     ^ ": "
                     ^ message
                     ^ "\n"))
               failures)
          ; raise Fail "Scanning error."
          )
    val ast = Ast.parse justTokens
  in
    Interpreter.interpret context ast
  end

fun runFile filename =
  let val program = TextIO.input (TextIO.openIn filename) in
    run (Context.make ()) program
  end

fun runPrompt () =
  let val context = Context.make () in
    while true do
      let val maybeLine = TextIO.inputLine TextIO.stdIn in
        case maybeLine of
          SOME line => run context line
        | NONE => OS.Process.exit OS.Process.success
      end
  end

val () =
  case CommandLine.arguments () of
    [] => runPrompt ()
  | s :: [] => runFile s
  | _ => (print "Usage: smlox [script]\n"; OS.Process.exit OS.Process.failure)
