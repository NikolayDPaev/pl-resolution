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
        Quantification q x f -> Quantification q x (eliminateImplAndEqv f)
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
                
                Quantification Exists x f -> Quantification ForAll x (moveNegations (negate f))
                Quantification ForAll x f -> Quantification Exists x (moveNegations (negate f))

        Predicate _ _ -> outerFormula
        Operation a op b -> Operation (moveNegations a) op (moveNegations b)
        Quantification q x f -> Quantification q x (moveNegations f)

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
                Quantification ForAll x f1 ->
                    let (newF, newL) = (skolemHelper l gen (x :: dependencies) substitutions f1)
                    in (Quantification ForAll x newF, newL)
                Quantification Exists x f1 -> 
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

toPNF : Language -> Formula -> (Formula, Language)
toPNF lang formula =
    let
        substituteVar : String -> String -> Formula -> Formula
        substituteVar var subVar f =
            case f of
                Predicate p terms -> Predicate p (List.map (replaceInTerm (Dict.singleton var (Variable subVar))) terms)
                Negation f1 -> negate (substituteVar var subVar f1)
                Operation f1 op f2 -> (Operation (substituteVar var subVar f1) op (substituteVar var subVar f2))
                Quantification q x f1 -> Quantification q x (substituteVar var subVar f1)
        generatorWithout : Set String -> Language -> Generator
        generatorWithout vars l = (createGenerator l.consts l.funcs (Set.union l.vars vars))
        makeUniqueBounded : Language -> Formula -> Set String -> (Formula, Set String, Language)
        makeUniqueBounded l f varsByFar =
            case f of
                Predicate _ terms ->
                    let vars = List.foldr (\ x acc -> Set.union (varsInTerm x) acc) Set.empty terms
                    in (f, vars, l)
                Negation f1 ->
                    makeUniqueBounded l f1 varsByFar
                Operation f1 op f2 ->
                    let (newF1, newVarsF1, newL1) = makeUniqueBounded l f1 varsByFar
                        (newF2, newVarsF2, newL2) = makeUniqueBounded newL1 f2 newVarsF1                        
                    in ((Operation newF1 op newF2), newVarsF2, newL2)
                Quantification q x f1 ->
                    if Set.member x varsByFar then
                        let
                            (newX, _) = getVar (generatorWithout varsByFar l)
                            newL = {l | vars = Set.insert newX l.vars}
                            newF1 = substituteVar x newX f1
                            newVarsByFar = Set.insert newX varsByFar
                        in ((Quantification q newX newF1), newVarsByFar, newL)
                    else (f, Set.insert x varsByFar, l)
        
        pullQuantors : Formula -> Formula
        pullQuantors f =
            case f of
                Negation (Quantification q x f1) -> Quantification q x (Negation (pullQuantors f1))
                Negation f1 -> Negation (pullQuantors f1)
                Operation (Quantification q1 x1 f1) op (Quantification q2 x2 f2) ->
                    Quantification q1 x1 (Quantification q2 x2 (Operation (pullQuantors f1) op (pullQuantors f2)))
                Operation (Quantification q1 x1 f1) op f2 ->
                    Quantification q1 x1 (Operation (pullQuantors f1) op (pullQuantors f2))
                Operation f1 op (Quantification q2 x2 f2) ->
                    Quantification q2 x2 (Operation (pullQuantors f1) op (pullQuantors f2))
                Operation f1 op f2 -> Operation (pullQuantors f1) op (pullQuantors f2)
                _ -> f
    
        (uniqueBoundedF, _, finalL) = makeUniqueBounded lang formula Set.empty
    in 
    (pullQuantors uniqueBoundedF, finalL)

-- in order to be correct, the provided formula must not have implication and equivalences
pNFtoCNF : Formula -> Formula
pNFtoCNF formula = 
    let
        noQuantorsToCNF : Formula -> Formula
        noQuantorsToCNF formula_ =
            case formula_ of
                Operation f1 And f2 -> Operation (noQuantorsToCNF f1) And (noQuantorsToCNF f2)
                Operation f1 Or f2 -> distribute (noQuantorsToCNF f1) (noQuantorsToCNF f2)
                _ -> formula_

        distribute : Formula -> Formula -> Formula
        distribute f1_ f2_ =
            case (f1_, f2_) of
                ((Operation f11 And f12), f2) -> Operation (distribute f11 f2) And (distribute f12 f2)
                (f1, (Operation f21 And f22)) -> Operation (distribute f1 f21) And (distribute f1 f22)
                (f1, f2) -> Operation f1 Or f2
    in
    case formula of
        Quantification q x f -> Quantification q x (pNFtoCNF f)
        _ -> formula


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

-- input must be formula with quantor prefix and matrix in CNF
toListOfDisjuncts : Formula -> List Disjunct
toListOfDisjuncts formula =
    let
        removeQuantorPrefix : Formula -> Formula
        removeQuantorPrefix f =
            case f of
                Quantification _ _ f1 -> removeQuantorPrefix f1
                _ -> f

        literalToDisjunct : Formula -> Disjunct
        literalToDisjunct f =
            case f of
                Predicate p terms -> [PositivePredicate p terms]
                Negation (Predicate p terms) -> [NegativePredicate p terms]
                _ -> []

        cnfToListOfDisjuncts : Formula -> List Disjunct
        cnfToListOfDisjuncts f =
            case f of 
                Operation f1 And f2 -> List.append (cnfToListOfDisjuncts f1) (cnfToListOfDisjuncts f2)
                Operation f1 Or f2 -> 
                    case (cnfToListOfDisjuncts f1, cnfToListOfDisjuncts f2) of
                        ([d1], [d2]) -> [List.append d1 d2]
                        _ -> []
                _ -> [literalToDisjunct f]

    in
    formula
    |> removeQuantorPrefix
    |> cnfToListOfDisjuncts
