module DisjunctSet exposing (..)

import Disjunct exposing (Disjunct)
import Set.Any exposing (AnySet)

type alias DisjunctSet = AnySet String Disjunct

fromList : List Disjunct -> DisjunctSet
fromList = Set.Any.fromList Disjunct.toString

toList : DisjunctSet -> List Disjunct
toList = Set.Any.toList

union : DisjunctSet -> DisjunctSet -> DisjunctSet 
union = Set.Any.union

toString : DisjunctSet -> String
toString d =
    let
       listOfDisjunsts = Set.Any.toList d 
    in "{" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (Disjunct.toString x) ++ ", ") "" listOfDisjunsts) ++ "}"

empty : DisjunctSet
empty = Set.Any.empty Disjunct.toString

equal : DisjunctSet -> DisjunctSet -> Bool
equal = Set.Any.equal

insert : Disjunct -> DisjunctSet -> DisjunctSet
insert = Set.Any.insert

any : (Disjunct -> Bool) -> DisjunctSet -> Bool
any = Set.Any.any
