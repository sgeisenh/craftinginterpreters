structure Parser :> PARSER =
  struct
    open Common

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
      Assign of (string * expr annotated)
    | Binary of (binary_operator * expr annotated * expr annotated)
    | Call of (expr annotated * expr annotated list)
    | Grouping of expr annotated
    | Literal of literal
    | Logical of (logical_operator * expr annotated * expr annotated)
    | Unary of (unary_operator * expr annotated)
    | Variable of string
    datatype statement =
      Block of statement annotated list
    | Expression of expr annotated
    | Function of (string * string list * statement annotated list)
    | If of (expr * statement annotated * statement annotated option)
    | While of (expr annotated * statement annotated)
    | Print of expr annotated
    | Return of expr annotated
    | Var of (string * expr annotated)

    fun matchTypes types tokens =
      case tokens of
        [] => NONE
      | token :: tokens =>
          if
            List.exists (fn typ => Scanner.tokenEqual (typ, # value token))
              types
          then
            SOME (token, tokens)
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
      let
        val (left, tokens) = next (tokens)
        val {location = leftLocation, ...} = left
      in
        case matchTypes types tokens of
          NONE => (left, tokens)
        | SOME (token, tokens) =>
            let
              val (right, tokens) = parseBinaryLevel types next tokens
              val {location = rightLocation, ...} = right
              val operator = tokenToBinop token
              val merged_location =
                merge_locations [leftLocation, rightLocation]
            in
              ( { location = merged_location
                , value = Binary (operator, left, right)
                }
              , tokens
              )
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
          let val (statement, tokens) = parseStatement tokens in
            parseStatements' (tokens, (statement :: acc))
          end
    and parseStatement tokens =
      case tokens of
        {value = Scanner.Fun, ...} :: _ => parseFunction ("function", tokens)
      | {value = Scanner.Var, ...} :: _ => parseVarDeclaration tokens
      | {value = Scanner.For, ...} :: _ => parseForStatement tokens
      | {value = Scanner.If, ...} :: _ => parseIfStatement tokens
      | {value = Scanner.While, ...} :: _ => parseWhileStatement tokens
      | {value = Scanner.Return, ...} :: _ => parseReturnStatement tokens
      | {value = Scanner.Print, ...} :: _ => parsePrintStatement tokens
      | {value = Scanner.LeftBrace, ...} :: _ => parseBlock tokens
      | _ => parseExpressionStatement tokens
    and parseFunction (kind, tokens) =
      case tokens of
        {location = kwLocation, ...} :: { value = Scanner.Identifier name
                                        , location = identLocation
                                        } :: tokens =>
          (case tokens of
             {value = Scanner.LeftParen, ...} :: tokens =>
               let
                 val (parameters, tokens) = parseParameters tokens
                 val (body, tokens, blockLocation) =
                   case tokens of
                     {value = Scanner.LeftBrace, ...} :: _ =>
                       (case parseBlock tokens of
                          ( {value = Block body, location = blockLocation}
                          , tokens
                          ) =>
                            (body, tokens, blockLocation)
                        | _ => raise Fail "Unreachable.")
                   | _ => raise Fail ("Expect '{' before " ^ kind ^ " body.")
               in
                 ( { value = Function (name, parameters, body)
                   , location = merge_locations [kwLocation, blockLocation]
                   }
                 , tokens
                 )
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
        {location = kwLocation, ...} :: {value = Scanner.Identifier ident, ...} :: tokens =>
          (case matchTypes [Scanner.Equal] tokens of
             NONE =>
               let
                 val statement =
                   Var (ident, {value = Literal Nil, location = Unknown})
                 val (semiLocation, tokens) =
                   case tokens of
                     {value = Scanner.Semicolon, location = semiLocation} :: tokens =>
                       (semiLocation, tokens)
                   | _ => raise Fail "Expect ';' after variable declaration."
               in
                 ( { value = statement
                   , location = merge_locations [kwLocation, semiLocation]
                   }
                 , tokens
                 )
               end
           | SOME (_, tokens) =>
               let
                 val (expr, tokens) = parseExpression tokens
                 val (semiLocation, tokens) =
                   case matchTypes [Scanner.Semicolon] tokens of
                     NONE => raise Fail "Expect ';' after variable declaration."
                   | SOME ({location = semiLocation, ...}, tokens) =>
                       (semiLocation, tokens)
               in
                 ( { value = Var (ident, expr)
                   , location = merge_locations [kwLocation, semiLocation]
                   }
                 , tokens
                 )
               end)
      | _ => raise Fail "Expect variable name."
    and parseForStatement tokens =
      case tokens of
        {location = kwLocation, ...} :: {value = Scanner.LeftParen, ...} :: tokens =>
          let
            val (initializer : statement annotated option, tokens) =
              case tokens of
                {value = Scanner.Semicolon, ...} :: tokens => (NONE, tokens)
              | {value = Scanner.Var, ...} :: _ =>
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
            val {location = bodyLocation, ...} = body
            val body =
              case increment of
                NONE => body
              | SOME increment =>
                  let val {location= incrLocation, ...} = increment in
                  { location = Unknown
                  , value = Block [body, {value = Expression increment,
                  location=incrLocation}]
                  }
                  end
            val condition =
              case condition of
                NONE => {location = Unknown, value = Literal True}
              | SOME condition => condition
            val body = While (condition, body)
            val body =
              case initializer of
                NONE => body
              | SOME initializer =>
                  { location = merge_locations [kwLocation, bodyLocation]
                  , value = Block [initializer, body]
                  }
          in
            (body, tokens)
          end
      | _ => raise Fail "Expect '(' after 'for'."
    and parseIfStatement tokens =
      case tokens of
        {location = kwLocation, ...} :: {value = Scanner.LeftParen, ...} :: tokens =>
          let val (condition, tokens) = parseExpression tokens in
            case tokens of
              {value = Scanner.RightParen, ...} :: tokens =>
                let
                  val (thenBranch, tokens) = parseStatement tokens
                  val {location = thenLocation, ...} = thenBranch
                  val (elseBranch, tokens) =
                    case tokens of
                      {value = Scanner.Else, ...} :: tokens =>
                        let val (elseBranch, tokens) = parseStatement tokens in
                          (SOME elseBranch, tokens)
                        end
                    | _ => (NONE, tokens)
                  val restLocation =
                    case elseBranch of
                      SOME {location, ...} => location
                    | NONE => thenLocation
                in
                  ( { value = If (condition, thenBranch, elseBranch)
                    , location = merge_locations [kwLocation, restLocation]
                    }
                  , tokens
                  )
                end
            | _ => raise Fail "Expect ')' after if condition."
          end
      | _ => raise Fail "Expect '(' after 'if'."
    and parseWhileStatement tokens =
      case tokens of
        {location = kwLocation, ...} :: {value = Scanner.LeftParen, ...} :: tokens =>
          let val (condition, tokens) = parseExpression tokens in
            case tokens of
              {value = Scanner.RightParen, ...} :: tokens =>
                let
                  val (body, tokens) = parseStatement tokens
                  val {location, ...} = body
                in
                  ( { value = While (condition, body)
                    , location = merge_locations [kwLocation, location]
                    }
                  , tokens
                  )
                end
            | _ => raise Fail "Expect ')' after condition."
          end
      | _ => raise Fail "Expect '(' after 'while'."
    and parseReturnStatement tokens =
      case tokens of
        {location = kwLocation, ...} :: { value = Scanner.Semicolon
                                        , location = semiLocation
                                        } :: tokens =>
          ( { value = Return (Literal Nil)
            , location = merge_locations [kwLocation, semiLocation]
            }
          , tokens
          )
      | _ =>
          let val (expression, tokens) = parseExpression tokens in
            case tokens of
              {value = Scanner.Semicolon, location = semiLocation} :: tokens =>
                ( { value = Return expression
                  , location = merge_locations [kwLocation, semiLocation]
                  }
                , tokens
                )
            | _ => raise Fail "Expect ';' after return value."
          end
    and parsePrintStatement tokens =
      case tokens of
        {location = kwLocation, ...} :: _ =>
          let
            val (expr, tokens) = parseExpression tokens
            val tokens =
              case matchTypes [Scanner.Semicolon] tokens of
                NONE => raise Fail "Expect ';' after value."
              | SOME (_, tokens) => tokens
          in
            (Print expr, tokens)
          end
      | _ => raise Fail "unreachable"
    and parseBlock tokens = parseBlock' tokens []
    and parseBlock' tokens acc =
      case tokens of
        {location = lBraceLocation, ...} :: tokens =>
          (case tokens of
             [] => raise Fail "Expect '}' after block."
           | {value = Scanner.RightBrace, location = rBraceLocation} :: tokens =>
               ( { value = Block (List.rev acc)
                 , location = merge_locations [lBraceLocation, rBraceLocation]
                 }
               , tokens
               )
           | _ =>
               let val (statement, tokens) = parseStatement tokens in
                 parseBlock' tokens (statement :: acc)
               end)
      | _ => raise Fail "unreachable"
    and parseExpressionStatement tokens =
      let
        val (expr, tokens) =
          parseExpression tokens var {location = exprLocation} = expr
        val (semiLocation, tokens) =
          case matchTypes [Scanner.Semicolon] tokens of
            NONE => raise Fail "Expect ';' after expression."
          | SOME ({location = semiLocation, ...}, tokens) =>
              (semiLocation, tokens)
      in
        ( { value = Expression expr
          , location = merge_locations [exprLocation, semiLocation]
          }
        , tokens
        )
      end
    and parseExpression tokens = parseAssignment tokens
    and parseAssignment tokens =
      let
        val (left, tokens) = parseOr tokens
        val {location = leftLocation, ...} = left
      in
        case matchTypes [Scanner.Equal] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let
              val (value, tokens) = parseAssignment tokens
              val {location = valueLocation, ...} = value
            in
              case left of
                {value = Variable ident, ...} =>
                  ( { value = Assign (ident, value)
                    , location = merge_locations [leftLocation, valueLocation]
                    }
                  , tokens
                  )
              | _ => raise Fail "Expected ident."
            end
      end
    and parseOr tokens =
      let
        val (left, tokens) = parseAnd tokens
        val {location = leftLocation, ...} = left
      in
        case matchTypes [Scanner.Or] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let
              val (right, tokens) = parseOr tokens
              val {location = rightLocation, ...} = right
            in
              ( { value = Logical (Or, left, right)
                , location = merge_locations [leftLocation, rightLocation]
                }
              , tokens
              )
            end
      end
    and parseAnd tokens =
      let
        val (left, tokens) = parseEquality tokens
        val {location = leftLocation, ...} = left
      in
        case matchTypes [Scanner.And] tokens of
          NONE => (left, tokens)
        | SOME (_, tokens) =>
            let
              val (right, tokens) = parseAnd tokens
              val {location = rightLocation, ...} = right
            in
              ( { value = Logical (And, left, right)
                , location = merge_locations [leftLocation, rightLocation]
                }
              , tokens
              )
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
      | SOME (token, tokens) =>
          let
            val (expr, tokens) = parseUnary tokens
            val {location = opLocation, ...} = token
            val {location = exprLocation, ...} = expr
          in
            ( { value = Unary (tokenToUnop token, expr)
              , location = merge_locations [opLocation, exprLocation]
              }
            , tokens
            )
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
        let val {location = calleeLocation, ...} = callee in
          case tokens of
            {value = Scanner.RightParen, location = rParenLocation} :: tokens =>
              ( { value = Call (callee, List.rev acc)
                , location = merge_locations [calleeLocation, rParenLocation]
                }
              , tokens
              )
          | _ =>
              let val (argument, tokens) = parseExpression tokens in
                case tokens of
                  {value = Scanner.RightParen, location = rParenLocation} :: tokens =>
                    ( { value = Call (callee, List.rev (argument :: acc))
                      , location =
                          merge_locations [calleeLocation, rParenLocation]
                      }
                    , tokens
                    )
                | {value = Scanner.Comma, ...} :: tokens =>
                    finishCall' (callee, tokens) (argument :: acc)
                | _ => raise Fail "Expect ')' after arguments."
              end
        end
    and parsePrimary tokens =
      case tokens of
        [] => raise Fail "Unable to parse primary from empty tokens"
      | token :: tokens =>
          case token of
            {value = Scanner.False, location} =>
              ({value = Literal False, location = location}, tokens)
          | {value = Scanner.True, location} =>
              ({value = Literal True, location = location}, tokens)
          | {value = Scanner.Nil, location} =>
              ({value = Literal Nil, location = loation}, tokens)
          | {value = Scanner.Number r, location} =>
              ({value = Literal (Number r), location = location}, tokens)
          | {value = Scanner.String s, location} =>
              ({value = Literal (String s), location = location}, tokens)
          | {value = Scanner.LeftParen, location = lParenLocation} =>
              let val (expr, tokens) = parseExpression tokens in
                case matchTypes [Scanner.RightParen] tokens of
                  NONE => raise Fail "Expect ')' after expression."
                | SOME ({location = rParenLocation, ...}, tokens) =>
                    ( { value = Grouping expr
                      , location =
                          merge_locations [lParenLocation, rParenLocation]
                      }
                    , tokens
                    )
              end
          | {value = Scanner.Identifier ident, location} =>
              ({value = Variable ident, location = location}, tokens)
          | _ => raise Fail "Expect expression."
  end
