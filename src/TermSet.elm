module TermSet exposing (..)

import Language exposing (Term)
import Set.Any exposing (AnySet)
import Language exposing (printTerm)

type alias TermSet = AnySet String Term

fromList : List Term -> TermSet
fromList = Set.Any.fromList printTerm

toList : TermSet -> List Term
toList = Set.Any.toList

union : TermSet -> TermSet -> TermSet 
union = Set.Any.union

toString : TermSet -> String
toString d =
    let
       listOfDisjunsts = Set.Any.toList d 
    in "{" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" listOfDisjunsts) ++ "}"

empty : TermSet
empty = Set.Any.empty printTerm

equal : TermSet -> TermSet -> Bool
equal = Set.Any.equal
