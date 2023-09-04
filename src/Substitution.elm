module Substitution exposing (..)

import Language exposing (Term)

type alias Substitution =  List (String, Term)

get : String -> Substitution -> Maybe Term
get x sub =
    sub
        |> List.filter (\ el -> x == Tuple.first el )
        |> List.head
        |> Maybe.map Tuple.second

insert : String -> Term -> Substitution -> Substitution
insert x t sub =
    sub ++ [(x, t)]

empty : Substitution
empty = []

singleton : String -> Term -> Substitution
singleton x t = [(x, t)]

toString : Substitution -> String
toString sub =
    "{" ++ String.dropRight 2 (List.foldl (\ (x, t) acc -> acc ++ x ++ "/" ++ Language.printTerm t ++ ", ") "" sub) ++ "}"

fromList : List (String, Term) -> Substitution
fromList = identity
