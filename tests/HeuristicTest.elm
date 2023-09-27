module HeuristicTest exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)

import Disjunct
import DisjunctSet
import Language exposing (Literal(..))
import Language exposing (Term(..))
import Heuristic exposing (..)

resolutionTest : Test
resolutionTest = describe "resolution test" [
        test "simple resolution" (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"]]
                result = Disjunct.empty
            in
            Expect.equalLists [result]
            (pseudoResolvents d1 d2)
        ),
        test "test substitution of rest" (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "r" [Variable "x"]]
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "x"]], NegativePredicate "r" [Variable "x"]])
            in
            Expect.equalLists
            [result]
            (pseudoResolvents d1 d2)
        ),
        test "multiple possible resolvents"  (\_ ->
            let
                d1 = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "y"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], NegativePredicate "q" [Variable "x"]]
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Variable "x"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Variable "x"], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [result1, result2]
            (pseudoResolvents d1 d2)
        ),
        test "no resolvents"  (\_ ->
            let
                d1 = Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
                d2 = Disjunct.fromList [NegativePredicate "p" [Constant "c"], PositivePredicate "q" [Variable "x"]]
            in
            Expect.equalLists
            []
            (pseudoResolvents d1 d2)
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
            [result]
            (pseudoColapses d)
        ),
        test "test substitution of rest" (\_ ->
            let
                d = Disjunct.fromList [PositivePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]], NegativePredicate "p" [Constant "c"]]
                result = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "x"]]])
            in
            Expect.equalLists
            [result]
            (pseudoColapses d)
        ),
        test "multiple possible colapses"  (\_ ->
            let
                d = Disjunct.fromList [PositivePredicate "p" [Variable "x"],
                                       PositivePredicate "q" [Function "f" [Variable "y"]],
                                       NegativePredicate "p" [Constant "c"],
                                       NegativePredicate "q" [Variable "x"]]
                result1 = (Disjunct.fromList [PositivePredicate "q" [Function "f" [Variable "y"]], NegativePredicate "q" [Variable "x"]])
                result2 = (Disjunct.fromList [PositivePredicate "p" [Variable "x"], NegativePredicate "p" [Constant "c"]])
            in
            Expect.equalLists
            [result1, result2]
            (pseudoColapses d)
        ),
        test "no colapses"  (\_ ->
            let
                d = Disjunct.fromList [NegativePredicate "p" [Variable "x"], PositivePredicate "q" [Function "f" [Variable "x"]]]
            in
            Expect.equalLists
            []
            (pseudoColapses d)
        )
    ]

minPseudoResolutionStepsTest : Test
minPseudoResolutionStepsTest = describe "minPseudoResolutionSteps test" [
        test "simple pseudo resolution steps of {{p(u), r(u, a)}, {q(w, f(u)), ¬r(w, u)}, {s(f(u))}, {¬p(v), ¬s(u)}, {¬q(v, u), ¬s(u)}}"
        (\_ -> let
                ds = (DisjunctSet.fromList
                    [
                        Disjunct.fromList [
                            PositivePredicate "p" [Variable "u"],
                            PositivePredicate "r" [Variable "u", Constant "a"]
                        ],
                        Disjunct.fromList [
                            PositivePredicate "q" [Variable "w", Function "f" [Variable "u"]],
                            NegativePredicate "r" [Variable "w", Variable "u"]
                        ],
                        Disjunct.fromList [
                            PositivePredicate "s" [Function "f" [Variable "u"]]
                        ],
                        Disjunct.fromList [
                            NegativePredicate "p" [Variable "v"],
                            NegativePredicate "s" [Variable "u"]
                        ],
                        Disjunct.fromList [
                            NegativePredicate "q" [Variable "v", Variable "u"],
                            NegativePredicate "s" [Variable "u"]
                        ]
                    ])
            in Expect.equal (minPseudoResolutionSteps ds 10) 4
        ),
        test "simple pseudo resolution steps of {{p(v), r(a, v)}, {q(v, g(v)), ¬s(v)}, {s(f(v))}, {¬p(g(v)), ¬s(v)}, {¬q(f(v), x), ¬r(v, x)}}"
        (\_ -> let
                ds = (DisjunctSet.fromList
                    [
                        Disjunct.fromList [
                            PositivePredicate "p" [Variable "v"],
                            PositivePredicate "r" [Constant "a", Variable "v"]
                        ],
                        Disjunct.fromList [
                            PositivePredicate "q" [Variable "v", Function "g" [Variable "v"]],
                            NegativePredicate "s" [Variable "v"]
                        ],
                        Disjunct.fromList [
                            PositivePredicate "s" [Function "f" [Variable "v"]]
                        ],
                        Disjunct.fromList [
                            NegativePredicate "p" [Function "g" [Variable "v"]],
                            NegativePredicate "s" [Variable "v"]
                        ],
                        Disjunct.fromList [
                            NegativePredicate "q" [Function "f" [Variable "v"], Variable "x"],
                            NegativePredicate "r" [Variable "v", Variable "x"]
                        ]
                    ])
            in Expect.equal (minPseudoResolutionSteps ds 10) 4
        )
    ]
