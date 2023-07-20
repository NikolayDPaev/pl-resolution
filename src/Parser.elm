module Parser exposing (parse, ParseError(..))
import Language exposing (Language)
import Language as L exposing (..)

import Set

type ParseError
    = ExpectedRightBracket
    | PredicateInTerm String
    | UnexpectedToken String
    | ExpectedVariable String
    | UnexpectedEnd

type Token
    = Var String
    | Const String
    | Func String
    | Pred String
    | LeftBracket
    | RightBracket
    | ForAll
    | Exists
    | And
    | Or
    | Impl
    | Eqv
    | Not
    | Comma

toString : Token -> String
toString token = 
    case token of
        Var str -> "Var: " ++ str
        Const str -> "Const: " ++ str
        Func str -> "Func: " ++ str
        Pred str -> "Pred: " ++ str
        LeftBracket -> "("
        RightBracket -> ")"
        ForAll -> "∀"
        Exists -> "∃"
        And -> "&"
        Or -> "∨"
        Eqv -> "⇔"
        Impl -> "⇒"
        Not -> "¬"
        Comma -> ","


tokenizer : Language -> String -> List Token
tokenizer lang str = 
    let basicTokens = String.foldr (\el acc ->
            case el of
                '(' -> LeftBracket :: acc
                ')' -> RightBracket :: acc
                '∀' -> ForAll :: acc
                '∃' -> Exists :: acc
                '&' -> And :: acc
                '∨' -> Or :: acc
                '⇔' -> Eqv :: acc
                '⇒' -> Impl:: acc
                '¬' -> Not :: acc
                ',' -> Comma :: acc
                c -> case acc of
                    Var restName :: rest -> Var (String.concat [String.fromChar c, restName]) :: rest
                    _ -> if Char.isAlphaNum c then Var (String.fromChar c) :: acc
                         else acc
            )[] str
    in basicTokens
        |> List.foldr (\token acc -> -- split words on whitespace
            case token of
                Var word ->
                    String.words word
                        |> List.map Var
                        |> List.foldr (::) acc
                _ -> token :: acc
            ) []
        |> List.map (\token -> -- map words to specific terms
            case token of
            Var string ->
                if Set.member string lang.consts then Const string
                else if Set.member string lang.funcs then Func string
                else if Set.member string lang.preds then Pred string
                else Var string
            _ -> token
        ) 

parseTerm : List Token -> Result ParseError (Term, List Token)
parseTerm tokens =
    case tokens of 
        Const string :: rest -> Ok (L.Constant string, rest)
        Var string :: rest -> Ok (L.Variable string, rest)
        Func string :: LeftBracket :: rest ->
            case parseTermList rest of
                Ok (terms, restTokens) ->
                    case restTokens of
                        RightBracket :: finalRest -> Ok (L.Function string terms, finalRest)
                        _ -> Err ExpectedRightBracket
                Err e -> Err e
        Pred p :: _ -> Err (PredicateInTerm p)
        t :: _ -> Err (UnexpectedToken (toString t))
        [] -> Err UnexpectedEnd
                

parseTermList : List Token -> Result ParseError (List Term, List Token)
parseTermList tokens = 
    case parseTerm tokens of
        Ok (firstTerm, rest) ->
            case rest of
                Comma :: restTokens ->
                    case parseTermList restTokens of
                        Ok (termList, finalRest) -> Ok (firstTerm :: termList, finalRest)
                        Err e -> Err e
                _ -> Ok ([firstTerm], rest)
        Err e -> Err e


parseFormula : List Token -> Result ParseError (Formula, List Token)
parseFormula tokens =
    let resultFirstFormula =
            case tokens of
                [] -> Err UnexpectedEnd
                LeftBracket :: rest ->
                    case parseFormula rest of
                        Ok (f, restTokens) ->
                            case restTokens of
                                RightBracket :: finalRest -> Ok (f, finalRest)
                                _ -> Err ExpectedRightBracket
                        Err e -> Err e
                Not :: rest ->
                    case parseFormula rest of
                        Ok ((L.Operation f1 op f2), restTokens) -> Ok ((L.Operation (L.Negation f1) op f2), restTokens)
                        Ok (formula, restTokens) -> Ok (L.Negation formula, restTokens)
                        Err e -> Err e 
                Pred x :: LeftBracket :: RightBracket :: rest ->
                    Ok (L.Predicate x [], rest)
                Pred x :: LeftBracket :: rest ->
                    case parseTermList rest of
                        Ok (terms, restTokens) ->
                            case restTokens of
                                RightBracket :: finalRest -> Ok (L.Predicate x terms, finalRest)
                                _ -> Err ExpectedRightBracket
                        Err e -> Err e 
                ForAll :: Var x :: rest -> 
                    case parseFormula rest of
                        Ok (f, restTokens) -> 
                            Ok (L.ForAll x f, restTokens)
                        Err e -> Err e
                ForAll :: t :: _ -> 
                    Err (ExpectedVariable (toString t))
                Exists :: Var x :: rest -> 
                    case parseFormula rest of
                        Ok (f, restTokens) -> 
                            Ok (L.Exists x f, restTokens)
                        Err e -> Err e
                Exists :: t :: _ -> 
                    Err (ExpectedVariable (toString t))
                t :: _ -> Err (UnexpectedToken (toString t))
    in
    Result.andThen (\(firstFormula, rest) ->
        case rest of
            And :: restTokens ->
                case parseFormula restTokens of
                    Ok (secondFormula, finalTokens) -> Ok ((L.Operation firstFormula L.And secondFormula),  finalTokens)
                    Err e -> Err e
            Or :: restTokens ->
                case parseFormula restTokens of
                    Ok (secondFormula, finalTokens) -> Ok ((L.Operation firstFormula L.Or secondFormula),  finalTokens)
                    Err e -> Err e
            Impl :: restTokens ->
                case parseFormula restTokens of
                    Ok (secondFormula, finalTokens) -> Ok ((L.Operation firstFormula L.Impl secondFormula),  finalTokens)
                    Err e -> Err e
            Eqv :: restTokens ->
                case parseFormula restTokens of
                    Ok (secondFormula, finalTokens) -> Ok ((L.Operation firstFormula L.Eqv secondFormula),  finalTokens)
                    Err e -> Err e
            _ -> Ok (firstFormula, rest)
    ) resultFirstFormula
        
parse : Language -> String -> Result ParseError Formula
parse lang string =
    case parseFormula (tokenizer lang string) of
        Ok (formula, []) -> Ok formula
        Ok (_, x :: _) -> Err (UnexpectedToken (toString x))
        Err e -> Err e
