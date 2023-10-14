module Heuristic exposing (..)

import Disjunct exposing (Disjunct)
import DisjunctSet exposing (DisjunctSet)
import Language exposing (Literal(..))
import ListHelperFunctions exposing (..)

-- heuristics counts the number of steps required to reach empty disjunct
-- if we are using pseudo resolution and pseudo colapses
-- ignoring the unification of the list of terms and taking in account only
-- the predicate symbol

-- for example p(x, a) and ¬p(f(y), b) will be allegeable for resolution

pseudoUnification : Literal -> Literal -> Bool
pseudoUnification l1 l2 =
    case (l1, l2) of
        (PositivePredicate p _, NegativePredicate q _) -> p == q
        (NegativePredicate p _, PositivePredicate q _) -> p == q
        _ -> False

pseudoResolvents : Disjunct -> Disjunct -> List Disjunct
pseudoResolvents d1 d2 =
    let
        d1List = Disjunct.toList d1
        d2List = Disjunct.toList d2
    in
    allPairs d1List d2List
        |> List.foldr (\ (l1, l2) acc ->
            if pseudoUnification l1 l2 then
                let
                        newD1 = Disjunct.remove l1 d1
                        newD2 = Disjunct.remove l2 d2
                        resolvent = Disjunct.union newD1 newD2
                    in
                    resolvent :: acc
            else acc
        ) []

pseudoColapses : Disjunct -> List Disjunct
pseudoColapses d =
    let
        dList = Disjunct.toList d
    in
    subsetsOf2 dList
        |> List.foldr (\ (l1, l2) acc ->
            if pseudoUnification l1 l2 then 
                let 
                    newD = Disjunct.remove l2 (Disjunct.remove l1 d)
                in
                newD :: acc
            else acc
        ) []

generateChildren : (DisjunctSet, Int) -> List (DisjunctSet, Int)
generateChildren (ds, steps) =
    let
        disjuncts = DisjunctSet.toList ds
    in
    subsetsOf2 disjuncts
        |> List.concatMap (\ (d1, d2) -> pseudoResolvents d1 d2)
        |> List.append (disjuncts |> List.concatMap (\ d -> pseudoColapses d))
        |> List.map (\ d -> (DisjunctSet.insert d ds, steps + 1))
        |> List.filter (\ (childDisjuncts, _) -> childDisjuncts /= ds)

final : DisjunctSet -> Bool
final ds = 
    DisjunctSet.any Disjunct.isEmpty ds

bfsLoop : List (DisjunctSet, Int) -> Int -> Int
bfsLoop queue maxDepth =
    case queue of
        (current, step) :: restQueue ->
            if final current then
                step
            else if step > maxDepth then
                step
            else
                let
                    children = generateChildren (current, step)
                    newQueue = List.append restQueue children
                in
                    bfsLoop newQueue maxDepth
        _ -> maxDepth

minPseudoResolutionSteps : DisjunctSet -> Int -> Int
minPseudoResolutionSteps ds maxDepth =
    bfsLoop [(ds, 0)] maxDepth
