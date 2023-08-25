module ResolutionStepTest exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)
import Dict

import ResolutionStep exposing (resolvents, LogEntry(..))
import Disjunct
import Language exposing (Literal(..))
import Language exposing (Term(..))
import ResolutionStep exposing (printLogEntry)
import ResolutionStep exposing (colapses)

logEntryToString : LogEntry -> String
logEntryToString le = 
    let (sub, step) = printLogEntry le
    in sub ++ " " ++ step

resolutionTest : Test
resolutionTest = describe "resolution test" [
        test "simple resolution" (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"]]
                result = Disjunct.empty
            in
            Expect.equalLists [(result, Res d1 d2 (Dict.singleton "x" (Constant "c")) result)]

            
            (resolvents d1 d2)
        ),
        test "test substitution of rest" (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "r" [Variable "x"]]
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Constant "c"]], NegativePredicate "r" [Constant "c"]])
            in
            Expect.equalLists
            [(result, (Res d1 d2 (Dict.singleton "x" (Constant "c")) result))]
            (resolvents d1 d2)
        ),
        test "multiple possible resolvents"  (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "y"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]]
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Constant "c"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Function "f" [Variable "y"]], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [(result1, (Res d1 d2 (Dict.singleton "x" (Constant "c")) result1)),
             (result2, (Res d1 d2 (Dict.singleton "x" (Function "f" [Variable "y"])) result2))
            ]
            (resolvents d1 d2)
        ),
        test "no resolvents"  (\_ ->
            let
                d1 = Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]]
            in
            Expect.equalLists
            []
            (resolvents d1 d2)
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
            [(result, (Col d (Dict.singleton "x" (Constant "c")) result))]
            (colapses d)
        ),
        test "test substitution of rest" (\_ ->
            let
                d = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]], NegativePredicate "p" [Constant "c"]]
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Constant "c"]]])
            in
            Expect.equalLists
            [(result, (Col d (Dict.singleton "x" (Constant "c")) result))]
            (colapses d)
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
            [(result1, (Col d (Dict.singleton "x" (Constant "c")) result1)),
             (result2, (Col d (Dict.singleton "x" (Function "f" [Variable "y"])) result2))]
            (colapses d)
        ),
        test "no colapses"  (\_ ->
            let
                d = Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
            in
            Expect.equalLists
            []
            (colapses d)
        )
    ]


