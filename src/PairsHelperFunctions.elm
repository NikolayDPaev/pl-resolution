module PairsHelperFunctions exposing (..)

subsetsOf2 : List a -> List (a, a)
subsetsOf2 list =
    case list of
        [] -> []
        x :: xs ->
            List.map (\y -> (x, y)) xs ++ subsetsOf2 xs

allPairs : List a -> List b -> List (a, b)
allPairs list1 list2 =
    List.concatMap (\x -> List.map (\y -> (x, y)) list2) list1
