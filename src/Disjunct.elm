module Disjunct exposing (..)

import Set.Any exposing (AnySet)
import Language exposing (Literal, literalToString)

type alias Disjunct = AnySet String Literal

fromList : List Literal -> Disjunct
fromList = Set.Any.fromList literalToString

union : Disjunct -> Disjunct -> Disjunct
union = Set.Any.union

toString : Disjunct -> String
toString d =
    if isEmpty d then "■"
    else
    let
       listOfDisjunsts = Set.Any.toList d 
    in "{" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (literalToString x) ++ ", ") "" listOfDisjunsts) ++ "}"

empty : Disjunct
empty = Set.Any.empty literalToString

isEmpty : Disjunct -> Bool
isEmpty = Set.Any.isEmpty

equal : Disjunct -> Disjunct -> Bool
equal = Set.Any.equal

singleton : Literal -> Disjunct
singleton l = Set.Any.singleton l literalToString

toList : Disjunct -> List Literal
toList = Set.Any.toList

remove : Literal -> Disjunct -> Disjunct
remove = Set.Any.remove

map : (Literal -> Literal) -> Disjunct -> Disjunct
map f d = Set.Any.map literalToString f d

size : Disjunct -> Int
size = Set.Any.size

type alias IndexedDisjunct = (Int, Disjunct)

indexedDisjunctToString : IndexedDisjunct -> String
indexedDisjunctToString (i, d) =
    "D" ++ String.fromInt i ++ " = " ++ (toString d)

indexToString : IndexedDisjunct -> String
indexToString (i, _) = "D" ++ String.fromInt i
