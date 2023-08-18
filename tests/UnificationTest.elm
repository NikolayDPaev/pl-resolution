module UnificationTest exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)
import Dict

import Unification exposing (unification)
import Language exposing (Literal(..))
import Language exposing (Term(..))

possibleUnificationTest : Test
possibleUnificationTest = describe "possible unification tests" [
    test "empty" (\ _ ->
        Expect.equal
        (Just [])
        ((unification (PositivePredicate "p" [Constant "c", Function "f" [Variable "x"]]) (NegativePredicate "p" [Constant "c", Function "f" [Variable "x"]]))
            |> Maybe.map Dict.toList
            )
        ),
    test "var with constant" (\ _ ->
        Expect.equal
        (Just [("x", Constant "c")])
        ((unification (PositivePredicate "p" [Variable "x"]) (NegativePredicate "p" [Constant "c"]))
            |> Maybe.map Dict.toList
            )
        ),
    test "var in function with function" (\ _ ->
        Expect.equal
        (Just [("x", Function "g" [Variable "y"])])
        ((unification (PositivePredicate "p" [Function "f" [Variable "x"]]) (NegativePredicate "p" [Function "f" [Function "g" [Variable "y"]]]))
            |> Maybe.map Dict.toList
            )
        ),
    test "double sub" (\ _ -> -- x -> g(y), z -> x = g(y)
        Expect.equal
        (Just [("x", Function "g" [Variable "y"]), ("z", Function "g" [Variable "y"])])
        ((unification (PositivePredicate "p" [Function "f" [Variable "x"], Variable "x"]) (NegativePredicate "p" [Function "f" [Function "g" [Variable "y"]], Variable "z"]))
            |> Maybe.map Dict.toList
            )
        ),
    test "multiple subs" (\ _ ->
        Expect.equal
        (Just [("x", Function "g" [Variable "y"]), ("z", Constant "c")])
        ((unification (PositivePredicate "p" [Function "f" [Variable "x"], Constant "c"]) (NegativePredicate "p" [Function "f" [Function "g" [Variable "y"]], Variable "z"]))
            |> Maybe.map Dict.toList
            )
        )
    ]

impossibleUnificationTest : Test
impossibleUnificationTest = describe "impossible unification tests" [
    test "different predicates" (\ _ ->
        Expect.equal
        Nothing
        ((unification (PositivePredicate "p" [Constant "c"]) (NegativePredicate "q" [Constant "c"]))
            |> Maybe.map Dict.toList
            )
        ),
    test "not dual predicates 1" (\ _ ->
        Expect.equal
        Nothing
        ((unification (PositivePredicate "p" [Constant "c"]) (PositivePredicate "p" [Constant "c"]))
            |> Maybe.map Dict.toList
            )
        ),
    test "not dual predicates 2" (\ _ ->
        Expect.equal
        Nothing
        ((unification (NegativePredicate "p" [Constant "c"]) (NegativePredicate "p" [Constant "c"]))
            |> Maybe.map Dict.toList
            )
        ),
    test "different function names" (\ _ ->
        Expect.equal
        Nothing
        ((unification (PositivePredicate "p" [Constant "c", Function "f" [Variable "x"]]) (NegativePredicate "p" [Constant "c", Function "g" [Variable "x"]]))
            |> Maybe.map Dict.toList
            )
        ),
    test "different terms count" (\ _ ->
        Expect.equal
        Nothing
        ((unification (PositivePredicate "p" [Function "f" [Variable "x"], Variable "x"]) (NegativePredicate "p" [Function "f" [Function "g" [Variable "y"]]]))
            |> Maybe.map Dict.toList
            )
        ),
    test "different terms count in a function" (\ _ ->
        Expect.equal
        Nothing
        ((unification (PositivePredicate "p" [Function "f" [Variable "x", Variable "y"], Variable "x"]) (NegativePredicate "p" [Function "f" [Function "g" [Variable "y"]], Variable "z"]))
            |> Maybe.map Dict.toList
            )
        ),
    test "impossible unification - x cannot be unified" (\ _ ->
        Expect.equal
        Nothing
        ((unification (PositivePredicate "p" [Function "f" [Variable "x"], Constant "c"]) (NegativePredicate "p" [Function "f" [Function "g" [Variable "y"]], Variable "x"]))
            |> Maybe.map Dict.toList
            )
        )
    ]
