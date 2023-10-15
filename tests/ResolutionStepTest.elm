module ResolutionStepTest exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)

import ResolutionStep exposing (resolvents, LogEntry(..))
import Disjunct
import Language exposing (Literal(..))
import Language exposing (Term(..))
import ResolutionStep exposing (colapses)
import Substitution

resolutionTest : Test
resolutionTest = describe "resolution test" [
        test "simple resolution" (\_ ->
            let
                d1 = (1, Disjunct.fromList [PositivePredicate "p" [Variable "x"]])
                d2 = (2, Disjunct.fromList [NegativePredicate "p" [Constant "c"]])
                result = Disjunct.empty
            in
            Expect.equalLists [(result, Res d1 d2 (Substitution.singleton "x" (Constant "c")) result)]
            (resolvents d1 d2)
        ),
        test "test substitution of rest" (\_ ->
            let
                d1 = (1, Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]])
                d2 = (2, Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "r" [Variable "x"]])
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Constant "c"]], NegativePredicate "r" [Constant "c"]])
            in
            Expect.equalLists
            [(result, (Res d1 d2 (Substitution.singleton "x" (Constant "c")) result))]
            (resolvents d1 d2)
        ),
        test "multiple possible resolvents"  (\_ ->
            let
                d1 = (1, Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "y"]]])
                d2 = (2, Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]])
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Constant "c"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Function "f" [Variable "y"]], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [(result1, (Res d1 d2 (Substitution.singleton "x" (Constant "c")) result1)),
             (result2, (Res d1 d2 (Substitution.singleton "x" (Function "f" [Variable "y"])) result2))
            ]
            (resolvents d1 d2)
        ),
        test "no resolvents"  (\_ ->
            let
                d1 = (1, Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]])
                d2 = (2, Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]])
            in
            Expect.equalLists
            []
            (resolvents d1 d2)
        ),
        test "resolution of {p(v), r(a, v)} and {¬q(f(v), x), ¬r(v, x)}" (\_ ->
            let
                d1 = (1, Disjunct.fromList [
                            PositivePredicate "p" [Variable "v"],
                            PositivePredicate "r" [Constant "a", Variable "v"]
                        ])
                d2 = (2, Disjunct.fromList [
                            NegativePredicate "q" [Function "f" [Variable "v"], Variable "x"],
                            NegativePredicate "r" [Variable "v", Variable "x"]
                        ])
                result = Disjunct.fromList [
                        PositivePredicate "p" [Constant "a"],
                        NegativePredicate "q" [Function "f" [Constant "a"], Constant "a"]
                    ]
            in
            Expect.equalLists
                [(result, (Res d1 d2 (Substitution.fromList [("v", Constant "a"), ("x", Constant "a")]) result))]
                (resolvents d1 d2)
        ),
        test "resolution of {q(x), ¬s(a, x)} and {s(x, z), ¬r(f(x), z)}" (\_ ->
            let
                d1 = (1, Disjunct.fromList [
                        PositivePredicate "q" [Variable "x"],
                        NegativePredicate "s" [Constant "a", Variable "x"]
                    ])
                d2 = (2, (Disjunct.fromList [
                        PositivePredicate "s" [Variable "x", Variable "z"],
                        NegativePredicate "r" [Function "f" [Variable "x"], Variable "z"]
                    ]))
                result = Disjunct.fromList [
                        PositivePredicate "q" [Constant "a"],
                        NegativePredicate "r" [Function "f" [Constant "a"], Constant "a"]
                    ]
            in
            Expect.equalLists
                [(result, (Res d1 d2 (Substitution.fromList [("x", Constant "a"), ("z", Constant "a")]) result))]
                (resolvents d1 d2)
        )
    ]

colapseTest : Test
colapseTest = describe "colapse test" [
        test "simple colapse" (\_ ->
            let
                d = (1, Disjunct.fromList [PositivePredicate "p" [Variable "x"], NegativePredicate "p" [Constant "c"]])
                result = Disjunct.empty
            in
            Expect.equalLists
            [(result, (Col d (Substitution.singleton "x" (Constant "c")) result))]
            (colapses d)
        ),
        test "test substitution of rest" (\_ ->
            let
                d = (1, Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]], NegativePredicate "p" [Constant "c"]])
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Constant "c"]]])
            in
            Expect.equalLists
            [(result, (Col d (Substitution.singleton "x" (Constant "c")) result))]
            (colapses d)
        ),
        test "multiple possible colapses"  (\_ ->
            let
                d = (1, Disjunct.fromList [PositivePredicate "p" [Variable "x"],
                                       PositivePredicate "q" [Function "f" [Variable "y"]],
                                       NegativePredicate "p" [Constant "c"],
                                       NegativePredicate "q" [Variable "x"]])
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Constant "c"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Function "f" [Variable "y"]], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [(result1, (Col d (Substitution.singleton "x" (Constant "c")) result1)),
             (result2, (Col d (Substitution.singleton "x" (Function "f" [Variable "y"])) result2))]
            (colapses d)
        ),
        test "no colapses"  (\_ ->
            let
                d = (1, Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]])
            in
            Expect.equalLists
            []
            (colapses d)
        )
    ]


