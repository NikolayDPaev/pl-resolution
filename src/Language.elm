module Language exposing (..)

import Generator exposing (..)
import Dict exposing (Dict)

type alias Language = { 
    vars : List String,
    consts : List String,
    preds : List String,
    funcs : List String
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

type Literal
    = PositivePredicate String (List Term)
    | NegativePredicate String (List Term)

printLiteral : Literal -> String
printLiteral literal = 
    case literal of
        PositivePredicate p terms -> 
            case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
                "" -> p
                list -> p ++ "(" ++ String.dropRight 2 list ++ ")"
        NegativePredicate p terms -> 
            case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
                "" -> "¬" ++ p
                list -> "¬" ++ p ++ "(" ++ String.dropRight 2 list ++ ")"

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

type Formula
    = Literal Literal
    | Negation Formula
    | Operation Formula Op Formula
    | Exists String Formula
    | ForAll String Formula

printFormula : Formula -> String
printFormula f =
    case f of
        Literal literal -> printLiteral literal
        Negation f1 -> "¬" ++ printFormula f1
        Operation f1 op f2 -> "(" ++ printFormula f1 ++ " " ++ printOp op ++ " " ++ printFormula f2 ++ ")"
        Exists x f1 -> "∃" ++ x ++ "(" ++ printFormula f1 ++ ")"
        ForAll x f1 -> "∀" ++ x ++ "(" ++ printFormula f1 ++ ")"

type alias Disjunct =
    List Literal

negate : Formula -> Formula
negate f =
    case f of
        Literal literal ->
            case literal of
                PositivePredicate p terms ->
                    Literal (NegativePredicate p terms)

                NegativePredicate p terms ->
                    Literal (PositivePredicate p terms)
        _ -> Negation f

eliminateImplAndEqv : Formula -> Formula
eliminateImplAndEqv formula =
    case formula of
        Operation a op b ->
            let
                newA = eliminateImplAndEqv a
                newB = eliminateImplAndEqv b
            in
            case op of
                Impl -> Operation (negate newA) Or newB
                Eqv -> Operation (Operation (negate newA) Or newB) And (Operation newA Or (negate newB))
                _ -> Operation newA op newB
        Negation f -> Negation (eliminateImplAndEqv f)
        Exists x f -> Exists x (eliminateImplAndEqv f)
        ForAll x f -> ForAll x (eliminateImplAndEqv f)
        _ -> formula

moveNegations : Formula -> Formula
moveNegations outerFormula =
    case outerFormula of
        Negation formula ->
            case formula of
                Literal _ -> negate formula
                Negation f -> moveNegations f
                Operation a op b ->
                    case op of
                        And ->
                            Operation (moveNegations (Negation a)) Or (moveNegations (Negation b))
                        Or ->
                            Operation (moveNegations (Negation a)) And (moveNegations (Negation b))
                        Impl ->
                            Operation (moveNegations a) And (moveNegations (Negation b))
                        Eqv ->
                            Operation (moveNegations (Negation a)) Eqv (moveNegations (Negation b))
                
                Exists x f -> ForAll x (moveNegations (Negation f))
                ForAll x f -> Exists x (moveNegations (Negation f))

        Literal _ -> outerFormula
        Operation a op b -> Operation (moveNegations a) op (moveNegations b)
        Exists x f -> Exists x (moveNegations f)
        ForAll x f -> ForAll x (moveNegations f)

skolemization : Language -> Formula -> (Formula, Language)
skolemization lang formula =
    let
        replaceInTerm : Dict String Term -> Term -> Term
        replaceInTerm substitutions term =
            case term of
                Variable var -> 
                    case Dict.get var substitutions of
                        Just subTerm -> subTerm
                        _ -> term
                Function f terms -> Function f (List.map (replaceInTerm substitutions) terms)
                _ -> term
        
        replaceInLiteral : Dict String Term -> Literal -> Literal
        replaceInLiteral substitutions literal =
            case literal of
                PositivePredicate p terms -> PositivePredicate p (List.map (replaceInTerm substitutions) terms)
                NegativePredicate p terms -> NegativePredicate p (List.map (replaceInTerm substitutions) terms)
                
        skolemHelper : Language -> Generator -> List String -> Dict String Term -> Formula -> (Formula, Language)
        skolemHelper l gen dependencies substitutions f =
            case f of
                Literal literal -> (Literal (replaceInLiteral substitutions literal), l)
                Negation f1 ->
                    let (newF, newL) = (skolemHelper l gen dependencies substitutions f1)
                    in (Negation newF, newL)
                Operation f1 op f2 ->
                    let (newF1, newL1) = (skolemHelper l gen dependencies substitutions f1)
                        (newF2, newL2) = (skolemHelper l gen dependencies substitutions f2)
                    in (Operation newF1 op newF2, {l | consts = List.append newL1.consts newL2.consts, funcs = List.append newL1.funcs newL2.funcs})
                ForAll x f1 ->
                    let (newF, newL) = (skolemHelper l gen (x :: dependencies) substitutions f1)
                    in (ForAll x newF, newL)
                Exists x f1 -> 
                    let (newTerm, newGen) = if List.isEmpty dependencies then Tuple.mapFirst Constant (getConst gen)
                            else Tuple.mapFirst (\ funcName -> Function funcName (List.map Variable dependencies)) (getFunc gen)
                        (newF, newL) = skolemHelper l newGen dependencies (Dict.insert x newTerm substitutions) f1
                    in 
                        case newTerm of 
                            Constant c -> (newF, {newL | consts = List.append newL.consts [c]})
                            Function func _ -> (newF, {newL | funcs = List.append newL.funcs [func]})
                            _ -> (newF, newL)
    in
    skolemHelper lang (createGenerator lang.consts lang.funcs lang.vars) [] Dict.empty formula

-- to CNF
-- to PNF
-- to List of Disjuncts 
