module Transformations exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

import Generator exposing (..)
import Language exposing (..)
import Disjunct exposing (Disjunct)
import DisjunctSet exposing (DisjunctSet)


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


toPNF : Language -> Formula -> (Formula, Language)
toPNF lang formula =
    let
        substituteVar : String -> String -> Formula -> Formula
        substituteVar var subVar f =
            case f of
                Predicate p terms -> Predicate p (List.map (replaceInTerm (Dict.singleton var (Variable subVar))) terms)
                Negation f1 -> negate (substituteVar var subVar f1)
                Operation f1 op f2 -> (Operation (substituteVar var subVar f1) op (substituteVar var subVar f2))
                Quantification q x f1 ->
                    if x == var then
                        f
                    else 
                        Quantification q x (substituteVar var subVar f1)

        generatorWithout : Set String -> Generator
        generatorWithout vars = (createGenerator Set.empty Set.empty vars)

        makeUniqueBounded : Language -> Formula -> Set String -> (Formula, Set String, Language)
        makeUniqueBounded l f varsByFar =
            case f of
                Predicate _ terms ->
                    let vars = List.foldr (\ x acc -> Set.union (varsInTerm x) acc) varsByFar terms
                    in (f, vars, l)
                Negation f1 ->
                    let (newF1, newVars, newL) = makeUniqueBounded l f1 varsByFar
                    in (negate newF1, newVars, newL)
                Operation f1 op f2 ->
                    let (newF1, newVarsF1, newL1) = makeUniqueBounded l f1 varsByFar
                        (newF2, newVarsF2, newL2) = makeUniqueBounded newL1 f2 newVarsF1
                    in ((Operation newF1 op newF2), newVarsF2, newL2)
                Quantification q x f1 ->
                    if Set.member x varsByFar then
                        let
                            newX = getVar (generatorWithout varsByFar) |> Tuple.first
                            newL = {l | vars = Set.insert newX l.vars}
                            newF1 = substituteVar x newX f1
                            (finalF1, finalVars, finalL) = makeUniqueBounded newL newF1 (Set.insert newX varsByFar)
                        in ((Quantification q newX finalF1), finalVars, finalL)
                    else
                        let 
                            (finalF1, finalVars, finalL) = makeUniqueBounded l f1 (Set.insert x varsByFar)
                        in ((Quantification q x finalF1), finalVars, finalL)
        
        pullQuantors : Formula -> Formula
        pullQuantors initF =
            let
                pullOnce : Formula -> Formula
                pullOnce f =
                    case f of
                        Negation (Quantification ForAll x f1) -> Quantification Exists x (Negation (pullQuantors f1))
                        Negation (Quantification Exists x f1) -> Quantification ForAll x (Negation (pullQuantors f1))
                        Negation f1 -> Negation (pullQuantors f1)
                        Operation (Quantification q1 x1 f1) op (Quantification q2 x2 f2) ->
                            Quantification q1 x1 (Quantification q2 x2 (pullQuantors (Operation (pullQuantors f1) op (pullQuantors f2))))
                        Operation (Quantification q1 x1 f1) op f2 ->
                            Quantification q1 x1 (pullQuantors (Operation (pullQuantors f1) op (pullQuantors f2)))
                        Operation f1 op (Quantification q2 x2 f2) ->
                            Quantification q2 x2 (pullQuantors (Operation (pullQuantors f1) op (pullQuantors f2)))
                        Operation f1 op f2 -> Operation (pullQuantors f1) op (pullQuantors f2)
                        Quantification q x f1 -> Quantification q x (pullQuantors f1)
                        _ -> f
                loop : Formula -> Formula
                loop f1 =
                    let 
                        newF = pullOnce f1
                    in
                    if newF /= f1 then
                        loop newF
                    else
                        newF
            in
            loop initF
    
        (uniqueBoundedF, _, modifiedL) = makeUniqueBounded lang formula Set.empty
    in 
    (pullQuantors uniqueBoundedF, modifiedL)

-- in order to be correct, the provided formula must not have implication and equivalences
-- and all negations must be at the level of the predicates
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
        _ -> noQuantorsToCNF formula


-- input must be formula with universal quantor prefix and matrix in CNF
toDisjunctSet : Formula -> DisjunctSet
toDisjunctSet formula =
    let
        removeQuantorPrefix : Formula -> Formula
        removeQuantorPrefix f =
            case f of
                Quantification _ _ f1 -> removeQuantorPrefix f1
                _ -> f

        literalToDisjunct : Formula -> Disjunct
        literalToDisjunct f =
            case f of
                Predicate p terms -> Disjunct.singleton (PositivePredicate p terms)
                Negation (Predicate p terms) -> Disjunct.singleton (NegativePredicate p terms)
                _ -> Disjunct.empty

        cnfToDisjunctSet : Formula -> DisjunctSet
        cnfToDisjunctSet f =
            case f of 
                Operation f1 And f2 -> DisjunctSet.union (cnfToDisjunctSet f1) (cnfToDisjunctSet f2)
                Operation f1 Or f2 -> 
                    case (DisjunctSet.toList (cnfToDisjunctSet f1), DisjunctSet.toList (cnfToDisjunctSet f2)) of
                        ([d1], [d2]) -> DisjunctSet.fromList [Disjunct.union d1 d2]
                        _ -> DisjunctSet.empty
                _ -> DisjunctSet.fromList [literalToDisjunct f]

    in
    formula
    |> removeQuantorPrefix
    |> cnfToDisjunctSet
