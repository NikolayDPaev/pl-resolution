module ResolutionStep exposing (..)

import Disjunct exposing (Disjunct)
import Unification exposing (unification, Substitution, replaceInLiteral, subToString)
import ListHelperFunctions exposing (..)

type LogEntry
    = Res Disjunct Disjunct Substitution Disjunct
    | Col Disjunct Substitution Disjunct

logEntryToString : LogEntry -> String
logEntryToString le = case le of
    Res d1 d2 sub resD -> subToString sub ++ " Res(" ++ Disjunct.toString d1 ++ ", " ++ Disjunct.toString d2 ++ ") = " ++ Disjunct.toString resD
    Col d sub resD -> subToString sub ++ " Col(" ++ Disjunct.toString d ++ ") = " ++ Disjunct.toString resD

resolvents : Disjunct -> Disjunct -> List (Disjunct, LogEntry)
resolvents d1 d2 =
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
                    (resolvent, (Res d1 d2 sub resolvent)) :: acc
        ) []
    
colapses : Disjunct -> List (Disjunct, LogEntry)
colapses d =
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
                    (colapse, Col d sub colapse) :: acc
        ) []
    