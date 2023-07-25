module Language exposing (..)

import Generator exposing (..)
import Set exposing (Set)

type alias Language = { 
    vars : Set String,
    consts : Set String,
    preds : Set String,
    funcs : Set String
    }

type Term
    = Constant String
    | Variable String
    | Function String (List Term)

printTerm : Term -> String
printTerm term =
    case term of
        Constant str -> str
        Variable str -> str
        Function str terms ->
            str ++ "(" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) ++ ")"

type Op
    = And
    | Or
    | Impl
    | Eqv

printOp : Op -> String
printOp op =
    case op of
        And -> "&"
        Or -> "∨"
        Impl -> "⇒"
        Eqv -> "⇔"

type Quantor = Exists | ForAll

type Formula
    = Predicate String (List Term)
    | Negation Formula
    | Operation Formula Op Formula
    | Quantification Quantor String Formula

printFormula : Formula -> String
printFormula f =
    case f of
        Predicate p terms -> case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
            "" -> p
            list -> p ++ "(" ++ String.dropRight 2 list ++ ")"
        Negation f1 -> "¬" ++ printFormula f1
        Operation f1 op f2 -> "(" ++ printFormula f1 ++ " " ++ printOp op ++ " " ++ printFormula f2 ++ ")"
        Quantification Exists x f1 -> "∃" ++ x ++ " " ++ printFormula f1
        Quantification ForAll x f1 -> "∀" ++ x ++ " " ++ printFormula f1

boundedVars : Formula -> Set String
boundedVars formula =
    case formula of
        Predicate _ _ -> Set.empty
        Negation f -> boundedVars f
        Operation f1 _ f2 -> Set.union (boundedVars f1) (boundedVars f2) 
        Quantification _ x f ->  Set.insert x (boundedVars f)

varsInTerm : Term -> Set String
varsInTerm term =
    case term of
        Constant _ -> Set.empty
        Variable v -> Set.singleton v
        Function _ terms -> List.foldr (\ x acc -> Set.union (varsInTerm x) acc) Set.empty terms

freeVars : Formula -> Set String
freeVars formula =
    case formula of
        Predicate _ terms -> List.foldr (\ x acc -> Set.union (varsInTerm x) acc) Set.empty terms
        Negation f -> freeVars f
        Operation f1 _ f2 -> Set.union (freeVars f1) (freeVars f2)
        Quantification _ x f ->  Set.remove x (freeVars f)

type Literal
    = PositivePredicate String (List Term)
    | NegativePredicate String (List Term)

literalToString : Literal -> String
literalToString literal = 
    case literal of
        PositivePredicate p terms -> 
            case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
                "" -> p
                list -> p ++ "(" ++ String.dropRight 2 list ++ ")"
        NegativePredicate p terms -> 
            case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
                "" -> "¬" ++ p
                list -> "¬" ++ p ++ "(" ++ String.dropRight 2 list ++ ")"

negateLiteral : Literal -> Literal
negateLiteral l =
    case l of    
        PositivePredicate p terms ->
            NegativePredicate p terms
        NegativePredicate p terms ->
            PositivePredicate p terms
