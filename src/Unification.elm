module Unification exposing (..)

import Dict exposing (Dict)
import Set

import Language exposing (Term(..))
import Language exposing (Literal(..))
import Language exposing (varsInTerm)
import Transformations exposing (replaceInTerm)

type alias Substitution =  Dict String Term

type Difference
    = Equal
    | DifferentSubTerms (Term, Term)
    | DifferentTerms

firstDifferenceInListOfTerms : List Term -> List Term -> Difference
firstDifferenceInListOfTerms terms1 terms2 =
    case (terms1, terms2) of
        ([], []) -> Equal
        (x::xs, y::ys) -> case firstDifferenceOfTerms x y of
            Nothing -> firstDifferenceInListOfTerms xs ys
            Just (t1, t2) -> DifferentSubTerms (t1, t2)
        (_, _) -> DifferentTerms

firstDifferenceOfTerms : Term -> Term -> Maybe (Term, Term)
firstDifferenceOfTerms t1 t2 =
    case (t1, t2) of
        (Variable x, Variable y) ->
            if x == y then Nothing
            else Just (t1, t2)
        (Constant x, Constant y) ->
            if x == y then Nothing
            else Just (t1, t2)
        (Function f terms1, Function g terms2) ->
            if f /= g then Just (t1, t2)
            else 
                case firstDifferenceInListOfTerms terms1 terms2 of
                    Equal -> Nothing
                    DifferentSubTerms (t11, t21) -> Just (t11, t21)
                    DifferentTerms -> Just (t1, t2)
        (_, _) -> Just (t1, t2)

replaceInListTerms : Substitution -> List Term -> List Term
replaceInListTerms sub list =
    List.map (replaceInTerm sub) list

unification : Literal -> Literal -> Maybe Substitution
unification p q = 
    let
        unificationHelper : List Term -> List Term -> Maybe Substitution -> Maybe Substitution
        unificationHelper l1 l2 maybeSub =
            Maybe.andThen (\ sub ->
                case firstDifferenceInListOfTerms l1 l2 of
                    Equal -> Just sub
                    DifferentTerms -> Nothing
                    DifferentSubTerms (t1, t2) ->
                        let
                            substituteAndCallRecursive : String -> Term -> Maybe Substitution
                            substituteAndCallRecursive x t =
                                unificationHelper
                                    (replaceInListTerms (Dict.fromList [(x, t)]) l1)
                                    (replaceInListTerms (Dict.fromList [(x, t)]) l2)
                                    (Just (Dict.insert x t sub))
                        in
                        case (t1, t2) of
                            (Variable x, Variable _) ->
                                substituteAndCallRecursive x t2
                            (Variable x, t) ->
                                if not (Set.member x (varsInTerm t))
                                then substituteAndCallRecursive x t
                                else Nothing
                            (t, Variable x) ->
                                if not (Set.member x (varsInTerm t))
                                then substituteAndCallRecursive x t
                                else Nothing
                            (_, _) -> Nothing
            ) maybeSub
    in
    case (p, q) of
        (PositivePredicate pName list1, NegativePredicate qName list2) ->
            if pName /= qName then Nothing
            else unificationHelper list1 list2 (Just Dict.empty)
        (NegativePredicate pName list1, PositivePredicate qName list2) ->
            if pName /= qName then Nothing
            else unificationHelper list1 list2 (Just Dict.empty)
        (_, _) -> Nothing
