module Parser exposing (..)
import Formula as F exposing (..)

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

-- tokenizer : String -> List Token
-- tokenizer str = 
--     String.foldr (\el acc ->
--         case el of
--             '(' -> LeftBracket :: acc
--             ')' -> RightBracket :: acc
--             '∀' -> ForAll :: acc
--             '∃' -> Exists :: acc
--             '&' -> And :: acc
--             '∨' -> Or :: acc
--             '↔' -> Eqv :: acc
--             '→' -> Impl:: acc
--             '¬' -> Not :: acc
--             ',' -> Comma :: acc
--             ' ' -> acc
--             c -> case acc of
--                 Name restName :: rest -> Name (String.concat [String.fromChar c, restName]) :: rest
--                 _ -> Name (String.fromChar c) :: acc

--     )[] str

parseTerm : List Token -> Maybe (Term, List Token)
parseTerm = Debug.todo "parse Term"

parseTermList : List Token -> Maybe (List Term, List Token)
parseTermList = Debug.todo "parse term, term, ..., term"

parseHelper : List Token -> Maybe (Formula, List Token)
parseHelper tokens =
    case tokens of
        [] -> Nothing
        LeftBracket :: rest ->
            case parseHelper rest of
                Just (f, restTokens) ->
                    case restTokens of
                        RightBracket :: finalRest -> Just (f, finalRest)
                        _ -> Nothing
                _ -> Nothing
        Not :: Pred x :: LeftBracket :: rest ->
            case parseTermList rest of
                Just (terms, restTokens) ->
                    case restTokens of
                        RightBracket :: finalRest -> Just (F.Literal (F.NegativePredicate x terms), finalRest)
                        _ -> Nothing
                _ -> Nothing 
        Pred x :: LeftBracket :: rest ->
            case parseTermList rest of
                Just (terms, restTokens) ->
                    case restTokens of
                        RightBracket :: finalRest -> Just (F.Literal (F.PositivePredicate x terms), finalRest)
                        _ -> Nothing
                _ -> Nothing 
        ForAll :: Var x :: rest -> 
            case parseHelper rest of
                Just (f, restTokens) -> 
                    Just (F.ForAll x f, restTokens)
                _ -> Nothing
        Exists :: Var x :: rest -> 
            case parseHelper rest of
                Just (f, restTokens) -> 
                    Just (F.Exists x f, restTokens)
                _ -> Nothing
        -- TODO : operators
        _ -> Nothing
