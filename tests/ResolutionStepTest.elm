module ResolutionStepTest exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)
import Dict

import ResolutionStep exposing (resolvents, LogEntry(..))
import Disjunct
import Language exposing (Literal(..))
import Language exposing (Term(..))
import ResolutionStep exposing (logEntryToString)
import ResolutionStep exposing (colapses)

resolutionTest : Test
resolutionTest = describe "resolution test" [
        test "simple resolution" (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"]]
                result = Disjunct.empty
            in
            Expect.equalLists
            [(Disjunct.toString result ++ " " ++ logEntryToString (Res d1 d2 (Dict.singleton "x" (Constant "c")) result))]
            (resolvents d1 d2
                |> List.map (\ (d, log) -> Disjunct.toString d ++ " " ++ logEntryToString log))
        ),
        test "test substitution of rest" (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "r" [Variable "x"]]
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Constant "c"]], NegativePredicate "r" [Constant "c"]])
            in
            Expect.equalLists
            [(Disjunct.toString result
                ++ " " ++ logEntryToString (Res d1 d2 (Dict.singleton "x" (Constant "c")) result))]
            (resolvents d1 d2
                |> List.map (\ (d, log) -> Disjunct.toString d ++ " " ++ logEntryToString log))
        ),
        test "multiple possible resolvents"  (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "y"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]]
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Constant "c"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Function "f" [Variable "y"]], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [(Disjunct.toString result1
                ++ " " ++ logEntryToString (Res d1 d2 (Dict.singleton "x" (Constant "c")) result1)),
            (Disjunct.toString result2
                ++ " " ++ logEntryToString (Res d1 d2 (Dict.singleton "x" (Function "f" [Variable "y"])) result2))]
            (resolvents d1 d2
                |> List.map (\ (d, log) -> Disjunct.toString d ++ " " ++ logEntryToString log))
        ),
        test "no resolvents"  (\_ ->
            let
                d1 = Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]]
            in
            Expect.equalLists
            []
            (resolvents d1 d2
                |> List.map (\ (d, log) -> Disjunct.toString d ++ " " ++ logEntryToString log))
        )
    ]

colapseTest : Test
colapseTest = describe "colapse test" [
        test "simple colapse" (\_ ->
            let
                d = Disjunct.fromList [PositivePredicate "p" [Variable "x"], NegativePredicate "p" [Constant "c"]]
                result = Disjunct.empty
            in
            Expect.equalLists
            [(Disjunct.toString result ++ " " ++ logEntryToString (Col d (Dict.singleton "x" (Constant "c")) result))]
            (colapses d
                |> List.map (\ (d_, log) -> Disjunct.toString d_ ++ " " ++ logEntryToString log))
        ),
        test "test substitution of rest" (\_ ->
            let
                d = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]], NegativePredicate "p" [Constant "c"]]
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Constant "c"]]])
            in
            Expect.equalLists
            [(Disjunct.toString result
                ++ " " ++ logEntryToString (Col d (Dict.singleton "x" (Constant "c")) result))]
            (colapses d
                |> List.map (\ (d_, log) -> Disjunct.toString d_ ++ " " ++ logEntryToString log))
        ),
        test "multiple possible colapses"  (\_ ->
            let
                d = Disjunct.fromList [PositivePredicate "p" [Variable "x"],
                                       PositivePredicate "q" [Function "f" [Variable "y"]],
                                       NegativePredicate "p" [Constant "c"],
                                       NegativePredicate "q" [Variable "x"]]
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Constant "c"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Function "f" [Variable "y"]], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [(Disjunct.toString result1
                ++ " " ++ logEntryToString (Col d (Dict.singleton "x" (Constant "c")) result1)),
            (Disjunct.toString result2
                ++ " " ++ logEntryToString (Col d (Dict.singleton "x" (Function "f" [Variable "y"])) result2))]
            (colapses d
                |> List.map (\ (d_, log) -> Disjunct.toString d_ ++ " " ++ logEntryToString log))
        ),
        test "no colapses"  (\_ ->
            let
                d = Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
            in
            Expect.equalLists
            []
            (colapses d
                |> List.map (\ (d_, log) -> Disjunct.toString d_ ++ " " ++ logEntryToString log))
        )
    ]


