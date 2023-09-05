module ResolutionStep exposing (..)

import Disjunct exposing (Disjunct, IndexedDisjunct)
import Unification exposing (unification, replaceInLiteral, subToString)
import ListHelperFunctions exposing (..)
import Substitution exposing (Substitution)

type LogEntry
    = Res IndexedDisjunct IndexedDisjunct Substitution Disjunct
    | Col IndexedDisjunct Substitution Disjunct

printLogEntry : LogEntry -> Int -> (String, String)
printLogEntry le index =
    case le of
        Res d1 d2 sub resD -> (subToString sub, "D" ++ String.fromInt index ++ " = Res(" ++ Disjunct.indexToString d1 ++ ", " ++ Disjunct.indexToString d2 ++ ") = " ++ Disjunct.toString resD)
        Col d sub resD -> (subToString sub, "D" ++ String.fromInt index ++ " = Col(" ++ Disjunct.indexToString d ++ ") = " ++ Disjunct.toString resD)

resolvents : IndexedDisjunct -> IndexedDisjunct -> List (Disjunct, LogEntry)
resolvents (i1, d1) (i2, d2) =
    let
        d1List = Disjunct.toList d1
        d2List = Disjunct.toList d2
    in
    allPairs d1List d2List
        |> List.foldr (\ (l1, l2) acc ->
            case unification l1 l2 of
                Nothing -> acc
                Just sub -> 
                    let
                        newD1 = Disjunct.remove l1 d1
                        newD2 = Disjunct.remove l2 d2
                        resolvent = Disjunct.union
                            (Disjunct.map (replaceInLiteral sub) newD1)
                            (Disjunct.map (replaceInLiteral sub) newD2)
                    in
                    (resolvent, (Res (i1, d1) (i2, d2) sub resolvent)) :: acc
        ) []
    
colapses : IndexedDisjunct -> List (Disjunct, LogEntry)
colapses (i, d) =
    let
        dList = Disjunct.toList d
    in
    subsetsOf2 dList
        |> List.foldr (\ (l1, l2) acc ->
            case unification l1 l2 of
                Nothing -> acc
                Just sub ->
                    let 
                        newD = Disjunct.remove l2 (Disjunct.remove l1 d)
                        colapse = Disjunct.map (replaceInLiteral sub) newD
                    in
                    (colapse, Col (i, d) sub colapse) :: acc
        ) []
    