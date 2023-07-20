module Language exposing (..)

import Generator exposing (..)
import Dict exposing (Dict)
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

type Formula
    = Predicate String (List Term)
    | Negation Formula
    | Operation Formula Op Formula
    | Exists String Formula
    | ForAll String Formula

printFormula : Formula -> String
printFormula f =
    case f of
        Predicate p terms -> case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
            "" -> p
            list -> p ++ "(" ++ String.dropRight 2 list ++ ")"
        Negation f1 -> "¬" ++ printFormula f1
        Operation f1 op f2 -> "(" ++ printFormula f1 ++ " " ++ printOp op ++ " " ++ printFormula f2 ++ ")"
        Exists x f1 -> "∃" ++ x ++ " " ++ printFormula f1
        ForAll x f1 -> "∀" ++ x ++ " " ++ printFormula f1

negate : Formula -> Formula
negate f =
    case f of
        Negation f1 -> f1
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
        Negation f -> negate (eliminateImplAndEqv f)
        Exists x f -> Exists x (eliminateImplAndEqv f)
        ForAll x f -> ForAll x (eliminateImplAndEqv f)
        _ -> formula

moveNegations : Formula -> Formula
moveNegations outerFormula =
    case outerFormula of
        Negation formula ->
            case formula of
                Predicate _ _ -> outerFormula
                Negation f -> moveNegations f
                Operation a op b ->
                    case op of
                        And ->
                            Operation (moveNegations (negate a)) Or (moveNegations (negate b))
                        Or ->
                            Operation (moveNegations (negate a)) And (moveNegations (negate b))
                        Impl ->
                            Operation (moveNegations a) And (moveNegations (negate b))
                        Eqv ->
                            Operation (moveNegations (negate a)) Eqv (moveNegations b)
                
                Exists x f -> ForAll x (moveNegations (negate f))
                ForAll x f -> Exists x (moveNegations (negate f))

        Predicate _ _ -> outerFormula
        Operation a op b -> Operation (moveNegations a) op (moveNegations b)
        Exists x f -> Exists x (moveNegations f)
        ForAll x f -> ForAll x (moveNegations f)

replaceInTerm : Dict String Term -> Term -> Term
replaceInTerm substitutions term =
    case term of
        Variable var -> 
            case Dict.get var substitutions of
                Just subTerm -> subTerm
                _ -> term
        Function f terms -> Function f (List.map (replaceInTerm substitutions) terms)
        _ -> term

skolemization : Language -> Formula -> (Formula, Language)
skolemization lang formula =
    let
        skolemHelper : Language -> Generator -> List String -> Dict String Term -> Formula -> (Formula, Language)
        skolemHelper l gen dependencies substitutions f =
            case f of
                Predicate p terms -> (Predicate p (List.map (replaceInTerm substitutions) terms), l)
                Negation f1 ->
                    let (newF, newL) = (skolemHelper l gen dependencies substitutions f1)
                    in (negate newF, newL)
                Operation f1 op f2 ->
                    let (newF1, newL1) = (skolemHelper l gen dependencies substitutions f1)
                        (newF2, newL2) = (skolemHelper l gen dependencies substitutions f2)
                    in (Operation newF1 op newF2, {l | consts = Set.union newL1.consts newL2.consts, funcs = Set.union newL1.funcs newL2.funcs})
                ForAll x f1 ->
                    let (newF, newL) = (skolemHelper l gen (x :: dependencies) substitutions f1)
                    in (ForAll x newF, newL)
                Exists x f1 -> 
                    let (newTerm, newGen) = if List.isEmpty dependencies then Tuple.mapFirst Constant (getConst gen)
                            else Tuple.mapFirst (\ funcName -> Function funcName (List.map Variable dependencies)) (getFunc gen)
                        (newF, newL) = skolemHelper l newGen dependencies (Dict.insert x newTerm substitutions) f1
                    in 
                        case newTerm of 
                            Constant c -> (newF, {newL | consts = Set.insert c newL.consts})
                            Function func _ -> (newF, {newL | funcs = Set.insert func newL.funcs})
                            _ -> (newF, newL)
    in
    skolemHelper lang (createGenerator lang.consts lang.funcs lang.vars) [] Dict.empty formula

toPrenexNormalForm : Language -> Formula -> (Formula, Language)
toPrenexNormalForm lang formula =
    let
        removeShadowedHelper : Language -> Generator -> List String -> List String -> Dict String Term -> Formula -> (Formula, Language)
        removeShadowedHelper l gen universalVars existentialVars substitutions f =
            case f of
                Predicate p terms -> (Predicate p (List.map (replaceInTerm substitutions) terms), l)
                Negation f1 ->
                    let (newF, newL) = (removeShadowedHelper l gen universalVars existentialVars substitutions f1)
                    in (negate newF, newL)
                Operation f1 op f2 ->
                    let (newF1, newL1) = (removeShadowedHelper l gen universalVars existentialVars substitutions f1)
                        (newF2, newL2) = (removeShadowedHelper l gen universalVars existentialVars substitutions f2)
                    in (Operation newF1 op newF2, {l | vars = Set.union newL1.vars newL2.vars})
                ForAll x f1 ->
                    if List.member x universalVars then
                        let (newVar, newGen) = getVar gen
                            (newF, newL) = removeShadowedHelper l newGen (newVar :: universalVars) existentialVars (Dict.insert x (Variable newVar) substitutions) f1
                        in (ForAll x newF, {newL | vars = Set.insert newVar newL.vars})
                    else let (newF, newL) = removeShadowedHelper l gen (x :: universalVars) existentialVars substitutions f1
                        in (ForAll x newF, newL)
                Exists x f1 ->
                    if List.member x universalVars then
                        let (newVar, newGen) = getVar gen
                            (newF, newL) = removeShadowedHelper l newGen universalVars (newVar :: existentialVars) (Dict.insert x (Variable newVar) substitutions) f1
                        in (Exists x newF, {newL | vars = Set.insert newVar newL.vars})
                    else let (newF, newL) = removeShadowedHelper l gen universalVars (x :: universalVars) substitutions f1
                        in (Exists x newF, newL)
        removeShadowed : Language -> Formula -> (Formula, Language)
        removeShadowed l f = removeShadowedHelper l (createGenerator lang.consts lang.funcs lang.vars) [] [] Dict.empty f

        substituteVar : String -> String -> Formula -> Formula
        substituteVar var subVar f =
            case f of
                Predicate p terms -> Predicate p (List.map (replaceInTerm (Dict.singleton var (Variable subVar))) terms)
                Negation f1 -> negate (substituteVar var subVar f1)
                Operation f1 op f2 -> (Operation (substituteVar var subVar f1) op (substituteVar var subVar f2))
                ForAll x f1 -> ForAll x (substituteVar var subVar f1)
                Exists x f1 -> Exists x (substituteVar var subVar f1)

        removePrefixHelper : Formula -> Formula
        removePrefixHelper f =
            case f of 
                ForAll _ f1 -> removePrefixHelper f1
                Exists _ f1 -> removePrefixHelper f1
                _ -> f
        
        -- remove prefix
        -- find one quantifier
        -- get it in front
        -- remove shadowed
        -- repeat  
    in
    Debug.todo "pnf"


-- to CNF
-- to PNF
-- to List of Disjuncts 

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

type alias Disjunct =
    List Literal

negateLiteral : Literal -> Literal
negateLiteral l =
    case l of    
        PositivePredicate p terms ->
            NegativePredicate p terms
        NegativePredicate p terms ->
            PositivePredicate p terms
