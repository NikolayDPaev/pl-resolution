module DisjunctSet exposing (..)

import Disjunct exposing (Disjunct, IndexedDisjunct)
import Set.Any exposing (AnySet)
type alias DisjunctSet = AnySet String IndexedDisjunct

toComparable : IndexedDisjunct -> String
toComparable = Tuple.second >> Disjunct.toString

fromList : List Disjunct -> DisjunctSet
fromList = List.indexedMap Tuple.pair >> Set.Any.fromList toComparable

toList : DisjunctSet -> List Disjunct
toList = Set.Any.toList >> List.map Tuple.second

toIndexedList : DisjunctSet -> List (Int, Disjunct)
toIndexedList = Set.Any.toList >> List.sortWith (\(i1, _) (i2, _) -> compare i1 i2)

union : DisjunctSet -> DisjunctSet -> DisjunctSet 
union = foldl insert

toString : DisjunctSet -> String
toString d =
    let
       listOfDisjunsts = toList d 
    in "{" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (Disjunct.toString x) ++ ", ") "" listOfDisjunsts) ++ "}"

toIndexedString : DisjunctSet -> String
toIndexedString ds =
    let
       listOfDisjunsts = toIndexedList ds 
    in "{" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ Disjunct.indexedDisjunctToString x ++ ", ") "" listOfDisjunsts) ++ "}"

empty : DisjunctSet
empty = Set.Any.empty toComparable

size : DisjunctSet -> Int
size = Set.Any.size

isEmpty : DisjunctSet -> Bool
isEmpty = Set.Any.isEmpty

equal : DisjunctSet -> DisjunctSet -> Bool
equal = Set.Any.equal

foldl : (Disjunct -> c -> c) -> c -> DisjunctSet -> c
foldl f = Set.Any.foldl (\ (_, d) acc -> f d acc)

insert : Disjunct -> DisjunctSet -> DisjunctSet
insert d ds = Set.Any.insert (Set.Any.size ds, d) ds

any : (Disjunct -> Bool) -> DisjunctSet -> Bool
any pred = Set.Any.any (\ (_, d) -> pred d)
