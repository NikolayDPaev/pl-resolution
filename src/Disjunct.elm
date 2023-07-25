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
    let
       listOfDisjunsts = Set.Any.toList d 
    in "{" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (literalToString x) ++ ", ") "" listOfDisjunsts) ++ "}"

empty : Disjunct
empty = Set.Any.empty literalToString

equal : Disjunct -> Disjunct -> Bool
equal = Set.Any.equal

singleton : Literal -> Disjunct
singleton l= Set.Any.singleton l literalToString
