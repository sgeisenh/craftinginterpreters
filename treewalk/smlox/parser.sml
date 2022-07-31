structure Parser :> PARSER =
  struct
    exception UnexpectedToken of (string * Scanner.token Common.annotated)

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
    datatype logical_operator = And | Or
    datatype unary_operator = Negative | Bang
    datatype expr =
      Assign of (string * expr)
    | Binary of (binary_operator * expr * expr)
    | Call of (expr * expr list)
    | Grouping of expr
    | Literal of literal
    | Logical of (logical_operator * expr * expr)
    | Unary of (unary_operator * expr)
    | Variable of string
    datatype statement =
      Block of statement list
    | Expression of expr
    | Function of (string * string list * statement list)
    | If of (expr * statement * statement option)
    | While of (expr * statement)
    | Print of expr
    | Return of expr
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
    fun logOpToString operator =
      case operator of
        Or => "Or"
      | And => "And"
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
      | Call (callee, arguments) =>
          "("
          ^ exprToString callee
          ^ String.concatWith " " (map exprToString arguments)
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
      | Logical (operator, left, right) =>
          "("
          ^ logOpToString operator
          ^ " "
          ^ exprToString left
          ^ " "
          ^ exprToString right
          ^ ")"

    fun matchTypes types tokens =
      case tokens of
        [] => NONE
      | token :: tokens' =>
          if
            List.exists (fn typ => Scanner.tokenEqual (typ, # value token))
              types
          then
            SOME (token, tokens')
          else
            NONE

    fun tokenToBinop annToken =
      let val {value, location} = annToken in
        case value of
          Scanner.Minus => Minus
        | Scanner.Plus => Plus
        | Scanner.Slash => Slash
        | Scanner.Star => Star
        | Scanner.BangEqual => BangEqual
        | Scanner.EqualEqual => EqualEqual
        | Scanner.Greater => Greater
        | Scanner.GreaterEqual => GreaterEqual
        | Scanner.Less => Less
        | Scanner.LessEqual => LessEqual
        | _ => raise UnexpectedToken ("non-logical binary operator", annToken)
      end

    fun tokenToUnop annToken =
      let val {value, location} = annToken in
        case value of
          Scanner.Minus => Negative
        | Scanner.Bang => Bang
        | _ => raise UnexpectedToken ("unary operator", annToken)
      end

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

    fun parse tokens =
      let val statements = parseStatements tokens in
        Common.Success statements
      end
    and parseStatements tokens = parseStatements' (tokens, [])
    and parseStatements' (tokens, acc) =
      case tokens of
        [] => List.rev acc
      | [{value = Scanner.Eof, ...}] => List.rev acc
      | _ =>
          let val (statement, tokens') = parseStatement tokens in
            parseStatements' (tokens', (statement :: acc))
          end
    and parseStatement tokens =
      case tokens of
        {value = Scanner.Fun, ...} :: tokens =>
          parseFunction ("function", tokens)
      | {value = Scanner.Var, ...} :: tokens' => parseVarDeclaration tokens'
      | {value = Scanner.For, ...} :: tokens' => parseForStatement tokens'
      | {value = Scanner.If, ...} :: tokens' => parseIfStatement tokens'
      | {value = Scanner.While, ...} :: tokens' => parseWhileStatement tokens'
      | {value = Scanner.Return, ...} :: tokens => parseReturnStatement tokens
      | {value = Scanner.Print, ...} :: tokens' => parsePrintStatement tokens'
      | {value = Scanner.LeftBrace, ...} :: tokens' => parseBlock tokens'
      | _ => parseExpressionStatement tokens
    and parseFunction (kind, tokens) =
      case tokens of
        {value = Scanner.Identifier name, ...} :: tokens =>
          (case tokens of
             {value = Scanner.LeftParen, ...} :: tokens =>
               let
                 val (parameters, tokens) = parseParameters tokens
                 val (body, tokens) =
                   case tokens of
                     {value = Scanner.LeftBrace, ...} :: tokens =>
                       (case parseBlock tokens of
                          (Block body, tokens) => (body, tokens)
                        | _ => raise Fail "Unreachable.")
                   | _ => raise Fail ("Expect '{' before " ^ kind ^ " body.")
               in
                 (Function (name, parameters, body), tokens)
               end
           | _ => raise Fail ("Expect '(' after " ^ kind ^ " name."))
      | _ => raise Fail ("Expect " ^ kind ^ " name.")
    and parseParameters tokens = parseParameters' tokens []
    and parseParameters' tokens acc =
      case tokens of
        {value = Scanner.RightParen, ...} :: tokens => (List.rev acc, tokens)
      | {value = Scanner.Identifier parameter, ...} :: tokens =>
          (case tokens of
             {value = Scanner.Comma, ...} :: tokens =>
               parseParameters' tokens (parameter :: acc)
           | {value = Scanner.RightParen, location} :: tokens =>
               parseParameters'
                 ({value = Scanner.RightParen, location = location} :: tokens)
                 (parameter :: acc)
           | _ => raise Fail "Only comma or rparen after param")
      | _ => raise Fail "Expect parameter name."
    and parseVarDeclaration tokens =
      case tokens of
        ({value = Scanner.Identifier ident, ...}) :: tokens' =>
          (case matchTypes [Scanner.Equal] tokens' of
             NONE =>
               let
                 val statement = Var (ident, Literal Nil)
                 val tokens =
                   case tokens' of
                     {value = Scanner.Semicolon, ...} :: tokens => tokens
                   | _ => raise Fail "Expect ';' after variable declaration."
               in
                 (statement, tokens)
               end
           | SOME (_, tokens') =>
               let
                 val (expr, tokens') = parseExpression tokens'
                 val tokens' =
                   case matchTypes [Scanner.Semicolon] tokens' of
                     NONE => raise Fail "Expect ';' after variable declaration."
                   | SOME (_, tokens') => tokens'
               in
                 (Var (ident, expr), tokens')
               end)
      | _ => raise Fail "Expect variable name."
    and parseForStatement tokens =
      case tokens of
        {value = Scanner.LeftParen, ...} :: tokens =>
          let
            val (initializer, tokens) =
              case tokens of
                {value = Scanner.Semicolon, ...} :: tokens => (NONE, tokens)
              | {value = Scanner.Var, ...} :: tokens =>
                  let val (initializer, tokens) = parseVarDeclaration tokens in
                    (SOME initializer, tokens)
                  end
              | _ =>
                  let
                    val (initializer, tokens) = parseExpressionStatement tokens
                  in
                    (SOME initializer, tokens)
                  end
            val (condition, tokens) =
              case tokens of
                {value = Scanner.Semicolon, ...} :: _ => (NONE, tokens)
              | _ =>
                  let val (condition, tokens) = parseExpression tokens in
                    (SOME condition, tokens)
                  end
            val tokens =
              case tokens of
                {value = Scanner.Semicolon, ...} :: tokens => tokens
              | _ => raise Fail "Expect ';' after loop condition."
            val (increment, tokens) =
              case tokens of
                {value = Scanner.RightParen, ...} :: _ => (NONE, tokens)
              | _ =>
                  let val (increment, tokens) = parseExpression tokens in
                    (SOME increment, tokens)
                  end
            val tokens =
              case tokens of
                {value = Scanner.RightParen, ...} :: tokens => tokens
              | _ => raise Fail "Expect ')' after for clauses."
            val (body, tokens) = parseStatement tokens
            val body =
              case increment of
                NONE => body
              | SOME increment => Block [body, Expression increment]
            val condition =
              case condition of
                NONE => Literal True
              | SOME condition => condition
            val body = While (condition, body)
            val body =
              case initializer of
                NONE => body
              | SOME initializer => Block [initializer, body]
          in
            (body, tokens)
          end
      | _ => raise Fail "Expect '(' after 'for'."
    and parseIfStatement tokens =
      case tokens of
        {value = Scanner.LeftParen, ...} :: tokens =>
          let val (condition, tokens) = parseExpression tokens in
            case tokens of
              {value = Scanner.RightParen, ...} :: tokens =>
                let
                  val (thenBranch, tokens) = parseStatement tokens
                  val (elseBranch, tokens) =
                    case tokens of
                      {value = Scanner.Else, ...} :: tokens =>
                        let val (elseBranch, tokens) = parseStatement tokens in
                          (SOME elseBranch, tokens)
                        end
                    | _ => (NONE, tokens)
                in
                  (If (condition, thenBranch, elseBranch), tokens)
                end
            | _ => raise Fail "Expect ')' after if condition."
          end
      | _ => raise Fail "Expect '(' after 'if'."
    and parseWhileStatement tokens =
      case tokens of
        {value = Scanner.LeftParen, ...} :: tokens =>
          let val (condition, tokens) = parseExpression tokens in
            case tokens of
              {value = Scanner.RightParen, ...} :: tokens =>
                let val (body, tokens) = parseStatement tokens in
                  (While (condition, body), tokens)
                end
            | _ => raise Fail "Expect ')' after condition."
          end
      | _ => raise Fail "Expect '(' after 'while'."
    and parseReturnStatement tokens =
      case tokens of
        {value = Scanner.Semicolon, ...} :: tokens =>
          (Return (Literal Nil), tokens)
      | _ =>
          let val (expression, tokens) = parseExpression tokens in
            case tokens of
              {value = Scanner.Semicolon, ...} :: tokens =>
                (Return expression, tokens)
            | _ => raise Fail "Expect ';' after return value."
          end
    and parsePrintStatement tokens =
      let
        val (expr, tokens') = parseExpression tokens
        val tokens' =
          case matchTypes [Scanner.Semicolon] tokens' of
            NONE => raise Fail "Expect ';' after value."
          | SOME (_, tokens') => tokens'
      in
        (Print expr, tokens')
      end
    and parseBlock tokens = parseBlock' tokens []
    and parseBlock' tokens acc =
      case tokens of
        [] => raise Fail "Expect '}' after block."
      | {value = Scanner.RightBrace, ...} :: tokens =>
          (Block (List.rev acc), tokens)
      | _ =>
          let val (statement, tokens) = parseStatement tokens in
            parseBlock' tokens (statement :: acc)
          end
    and parseExpressionStatement tokens =
      let
        val (expr, tokens') = parseExpression tokens
        val tokens' =
          case matchTypes [Scanner.Semicolon] tokens' of
            NONE => raise Fail "Expect ';' after expression."
          | SOME (_, tokens') => tokens'
      in
        (Expression expr, tokens')
      end
    and parseExpression tokens = parseAssignment tokens
    and parseAssignment tokens =
      let val (left, tokens) = parseOr tokens in
        case matchTypes [Scanner.Equal] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let val (value, tokens) = parseAssignment tokens in
              case left of
                Variable ident => (Assign (ident, value), tokens)
              | _ => raise Fail "Expected ident."
            end
      end
    and parseOr tokens =
      let val (left, tokens) = parseAnd tokens in
        case matchTypes [Scanner.Or] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let val (right, tokens) = parseOr tokens in
              (Logical (Or, left, right), tokens)
            end
      end
    and parseAnd tokens =
      let val (left, tokens) = parseEquality tokens in
        case matchTypes [Scanner.And] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let val (right, tokens) = parseAnd tokens in
              (Logical (And, left, right), tokens)
            end
      end
    and parseEquality tokens =
      parseBinaryLevel [Scanner.BangEqual, Scanner.EqualEqual] parseComparison
        tokens
    and parseComparison tokens =
      parseBinaryLevel
        [Scanner.Greater, Scanner.GreaterEqual, Scanner.Less, Scanner.LessEqual]
        parseTerm
        tokens
    and parseTerm tokens =
      parseBinaryLevel [Scanner.Minus, Scanner.Plus] parseFactor tokens
    and parseFactor tokens =
      parseBinaryLevel [Scanner.Slash, Scanner.Star] parseUnary tokens
    and parseUnary tokens =
      case matchTypes [Scanner.Bang, Scanner.Minus] tokens of
        NONE => parseCall tokens
      | SOME (token, tokens') =>
          let val (expr, tokens') = parseUnary tokens' in
            (Unary (tokenToUnop token, expr), tokens')
          end
    and parseCall tokens =
      let val (callee, tokens) = parsePrimary tokens in
        parseCalls (callee, tokens)
      end
    and parseCalls (callee, tokens) =
      case tokens of
        {value = Scanner.LeftParen, ...} :: tokens =>
          let val (callee, tokens) = finishCall (callee, tokens) in
            parseCalls (callee, tokens)
          end
      | _ => (callee, tokens)
    and finishCall (callee, tokens) = finishCall' (callee, tokens) []
    and finishCall' (callee, tokens) acc =
      if length acc >= 255 then
        raise Fail "Can't have more than 255 arguments."
      else
        case tokens of
          {value = Scanner.RightParen, ...} :: tokens =>
            (Call (callee, List.rev acc), tokens)
        | _ =>
            let val (argument, tokens) = parseExpression tokens in
              case tokens of
                {value = Scanner.RightParen, ...} :: tokens =>
                  (Call (callee, List.rev (argument :: acc)), tokens)
              | {value = Scanner.Comma, ...} :: tokens =>
                  finishCall' (callee, tokens) (argument :: acc)
              | _ => raise Fail "Expect ')' after arguments."
            end
    and parsePrimary tokens =
      case tokens of
        [] => raise Fail "Unable to parse primary from empty tokens"
      | token :: tokens' =>
          case token of
            {value = Scanner.False, ...} => (Literal False, tokens')
          | {value = Scanner.True, ...} => (Literal True, tokens')
          | {value = Scanner.Nil, ...} => (Literal Nil, tokens')
          | {value = Scanner.Number r, ...} => (Literal (Number r), tokens')
          | {value = Scanner.String s, ...} => (Literal (String s), tokens')
          | {value = Scanner.LeftParen, ...} =>
              let val (expr, tokens') = parseExpression tokens' in
                case matchTypes [Scanner.RightParen] tokens' of
                  NONE => raise Fail "Expect ')' after expression."
                | SOME (token, tokens') => (Grouping expr, tokens')
              end
          | {value = Scanner.Identifier ident, ...} => (Variable ident, tokens')
          | _ => raise Fail "Expect expression."
  end
