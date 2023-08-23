module TransformationsTest exposing (..)

import Set
import Expect exposing (..)
import Test exposing (Test, test, describe)

import Language exposing (..)
import Transformations exposing (..)
import DisjunctSet
import Disjunct
import Html.Attributes exposing (lang)

printTest : Test
printTest = describe "Printing tests" [
    test "And and Or" (\_ ->
        Expect.equal
        "((p(c, x) ∨ ¬q(f(c))) & q(g(y, f(c, x))))"
        (printFormula (Operation 
            (Operation 
                (Predicate "p" [Constant "c", Variable "x"])
                Or 
                (Negation (Predicate "q" [Function "f" [Constant "c"]])))
            And
            (Predicate "q" [Function "g" [Variable "y", Function "f" [Constant "c", Variable "x"]]])))
    ),
    test "Impl and Eqv" (\_ ->
        Expect.equal
        "((p(a, b) ⇔ ¬q(f(c))) ⇒ ¬q(g(y, f(c, x))))"
        (printFormula (Operation 
            (Operation 
                (Predicate "p" [Constant "a", Constant "b"])
                Eqv 
                (Negation (Predicate "q" [Function "f" [Constant "c"]])))
            Impl 
                (Negation (Predicate "q" [Function "g" [Variable "y", Function "f" [Constant "c", Variable "x"]]]))))
    ),
    test "quantors" (\_ ->
        Expect.equal
        "∀x ∃y (∀z p(z, y) ⇒ p(z, x))"
        (printFormula (Quantification ForAll "x" ( Quantification Exists "y" (Operation (Quantification ForAll "z" (Predicate "p" [Variable "z", Variable "y"])) Impl (Predicate "p" [Variable "z", Variable "x"])))))
    )
    ]

eliminateImplAndEqvTest : Test
eliminateImplAndEqvTest =
    test "eliminateImplEqvTest" (\_ ->
        Expect.equal
        (Operation 
            (Negation (Operation 
                (Operation
                    (Negation (Predicate "p" [Constant "a",Constant "b"]))
                    Or 
                    (Negation (Predicate "q" [Function "f" [Constant "c"]])))
                    And 
                (Operation 
                    (Predicate "p" [Constant "a",Constant "b"])
                    Or
                    (Predicate "q" [Function "f" [Constant "c"]]))))
            Or 
            (Negation (Predicate "q" [Function "g" [Variable "y",Function "f" [Constant "c",Variable "x"]]])))
        (eliminateImplAndEqv (Operation 
            (Operation 
                (Predicate "p" [Constant "a", Constant "b"])
                Eqv 
                (Negation (Predicate "q" [Function "f" [Constant "c"]])))
            Impl 
                (Negation (Predicate "q" [Function "g" [Variable "y", Function "f" [Constant "c", Variable "x"]]]))))
    )

moveNegationsTest : Test
moveNegationsTest = describe "moveNegationsTests" [
    test "deMorgan" (\_ ->
        Expect.equal
        (Operation 
                (Operation
                    (Negation (Predicate "p" [Constant "a"]))
                    Or
                    (Negation (Predicate "p" [Constant "b"])))
                And
                (Operation 
                    (Negation (Predicate "q" [Constant "c"]))
                    Or 
                    (Negation (Predicate "q" [Constant "a"]))))
        (moveNegations 
            (Negation 
                (Operation 
                (Operation
                    (Predicate "p" [Constant "a"])
                    And
                    (Predicate "p" [Constant "b"]))
                Or
                (Operation 
                    (Predicate "q" [Constant "c"])
                    And 
                    (Predicate "q" [Constant "a"]))))
        )
    ),
    test "impl and eqv" (\_ ->
        Expect.equal
        (Operation 
                (Operation
                    (Predicate "p" [Constant "a"])
                    And
                    (Negation (Predicate "p" [Constant "b"])))
                And
                (Operation 
                    (Negation (Predicate "q" [Constant "c"]))
                    Eqv 
                    (Predicate "q" [Constant "a"])))
        (moveNegations 
            (Negation 
                (Operation 
                (Operation
                    (Predicate "p" [Constant "a"])
                    Impl
                    (Predicate "p" [Constant "b"]))
                Or
                (Operation 
                    (Predicate "q" [Constant "c"])
                    Eqv 
                    (Predicate "q" [Constant "a"]))))
        )
    ),
    test "ForAll and Exist"  (\_ ->
        Expect.equal
        (Quantification Exists "x" 
                (Quantification ForAll "y"
                    (Operation 
                    (Negation (Predicate "p" [Constant "a"]))
                    And
                    (Quantification ForAll "y"
                    (Quantification Exists "z" (Predicate "p" [Constant "a"]))))))
        (moveNegations 
            (Negation (Quantification ForAll "x" 
                (Quantification Exists "y"
                    (Operation 
                    (Predicate "p" [Constant "a"])
                    Or
                    (Quantification Exists "y"
                    (Quantification ForAll "z" (Negation (Predicate "p" [Constant "a"]))))))))
        )
    )
    ]

skolemizationTest : Test
skolemizationTest =
    let lang = {vars = Set.fromList ["x", "y", "z"], 
                consts = Set.fromList ["a", "b"],
                preds = Set.fromList ["p", "q"],
                funcs = Set.fromList ["f", "g"]}
    in
    describe "skolemization tests" [
        test "noDependencies" (\_ ->
            Expect.equal
            ((Operation (Predicate "p" [Constant "c"])
                Impl
                (Predicate "q" [Constant "c"])), {lang | consts = Set.insert "c" lang.consts})
            (skolemization lang 
                (Quantification Exists "x" (Operation 
                    (Predicate "p" [Variable "x"])
                    Impl
                    (Predicate "q" [Variable "x"]))))
        ),
        test "MultipleDependencies" (\_ ->
            Expect.equal
            ((Quantification ForAll "y" (Operation
                (Predicate "p" [Function "h" [Variable "y"]])
                Impl
                (Predicate "q" [Function "h" [Variable "y"], Function "i" [Variable "y"]]))),
            {lang | funcs = Set.union (Set.fromList ["h", "i"]) lang.funcs})
            (skolemization lang 
                (Quantification ForAll "y" (Quantification Exists "x" (Operation
                    (Predicate "p" [Variable "x"])
                    Impl
                    (Quantification Exists "z" (Predicate "q" [Variable "x", Variable "z"]))))))
        )
    ]

toPNFTest : Test
toPNFTest =
    let lang = {vars = Set.fromList ["x", "y"], 
                consts = Set.fromList ["a", "b"],
                preds = Set.fromList ["p", "q"],
                funcs = Set.fromList ["f", "g"]}
    in
    describe "toPNFTests" [
        test "simple" (\_ ->
            Expect.equal
            ((Quantification ForAll "x"
                (Quantification ForAll "y"
                    (Operation 
                        (Predicate "p" [Variable "x"])
                        And 
                        (Predicate "q"[Variable "y"])))), lang)
            (toPNF lang 
            (Operation 
                (Quantification ForAll "x" (Predicate "p" [Variable "x"]))
                And
                (Quantification ForAll "x" (Predicate "q" [Variable "x"])))
            )
        ),
        test "shadowed" (\_ ->
            Expect.equal
            ((Quantification ForAll "x"
                (Quantification Exists "y"
                    (Quantification Exists "z"
                        (Quantification ForAll "u"
                            (Quantification Exists "v"
                                (Operation 
                                    (Predicate "p" [Variable "x", Variable "y"])
                                    Or
                                    (Negation (Operation
                                        (Predicate "q" [Variable "z", Variable "u"])
                                        And
                                        (Predicate "p" [Variable "v", Variable "v"])
                                    ))
                                )
                            )
                        )
                    )    
                )), {lang | vars = Set.fromList ["x", "y", "z", "u", "v"]})
            (toPNF lang 
            (Operation 
                (Quantification ForAll "x" (Quantification Exists "y"(Predicate "p" [Variable "x", Variable "y"])))
                Or
                (Negation
                (Quantification ForAll "y" (Quantification Exists "x" 
                    (Operation 
                        (Predicate "q" [Variable "y", Variable "x"])
                        And
                        (Quantification ForAll "x" (Predicate "p" [Variable "x", Variable "x"]))
                    )))))
            )
        ),
        test "pull quantors" (\_ ->
            let 
                lang1 = { consts = Set.fromList [], funcs = Set.fromList ["g"], preds = Set.fromList ["p","q", "r"], vars = Set.fromList ["x","y","t"] }
            in
            Expect.equal
            ((Quantification ForAll "y"
                (Quantification ForAll "t"
                    (Operation 
                        (Negation (Predicate "r" [Function "g" [Variable "y"]])) 
                        And  
                            (Operation 
                                (Negation (Predicate "p" [Function "g" [Variable "y"],Variable "t"]))
                                Or 
                                (Negation (Predicate "q" [Variable "y",Variable "t"])))))
                ), lang1)
            (toPNF lang1
            (Quantification ForAll "y"
                (Operation 
                    (Negation (Predicate "r" [Function "g" [Variable "y"]])) 
                    And 
                    (Quantification ForAll "t" 
                        (Operation 
                            (Negation (Predicate "p" [Function "g" [Variable "y"],Variable "t"]))
                            Or 
                            (Negation (Predicate "q" [Variable "y",Variable "t"])))))
                )
            )
        ),
        test "pull quantors 2" (\_ ->
            let 
                lang1 = { consts = Set.fromList [], funcs = Set.fromList ["f"], preds = Set.fromList ["p","q"], vars = Set.fromList ["x","y","z"] }
            in
            Expect.equal
            ((Quantification ForAll "x"
                (Quantification ForAll "y" 
                    (Quantification ForAll "z" 
                        (Quantification ForAll "u" 
                            (Operation 
                                (Negation (Predicate "p" [Variable "x",Function "f" [Variable "y"]]))
                                Or
                                (Operation 
                                    (Predicate "p" [Function "f" [Variable "y"],Variable "x"])
                                    Or
                                    (Negation (Predicate "p" [Variable "u",Variable "z"])))))))),
            {lang1 | vars = Set.insert "u" (Set.insert "z" lang1.vars)})
            (toPNF lang1
            (Quantification ForAll "x" 
                (Quantification ForAll "y" 
                    (Operation 
                        (Negation (Predicate "p" [Variable "x",Function "f" [Variable "y"]]))
                        Or
                        (Operation 
                            (Predicate "p" [Function "f" [Variable "y"],Variable "x"])
                            Or 
                            (Quantification ForAll "y" 
                                (Quantification ForAll "x" 
                                    (Negation (Predicate "p" [Variable "x",Variable "y"])))))))
            )
            )
        )
    ]

pNFToCNFTest : Test
pNFToCNFTest = describe "pNFToCNFTests" [
    test "1" (\_ ->
        Expect.equal 
        (Quantification ForAll "x"
            (Quantification Exists "y"
                (Quantification Exists "z"
                    (Quantification ForAll "u"
                        (Quantification Exists "v"
                            (Operation
                                (Operation 
                                    (Predicate "p" [Variable "x", Variable "y"])
                                    Or
                                    (Predicate "q" [Variable "z", Variable "u"])
                                )
                            And
                                (Operation
                                    (Predicate "p" [Variable "x", Variable "y"])
                                    Or
                                    (Predicate "p" [Variable "v", Variable "v"])
                                )
                            )
                        )
                    )
                )
            )
        )
        (pNFtoCNF 
            (Quantification ForAll "x"
                (Quantification Exists "y"
                    (Quantification Exists "z"
                        (Quantification ForAll "u"
                            (Quantification Exists "v"
                                (Operation 
                                    (Predicate "p" [Variable "x", Variable "y"])
                                    Or
                                    (Operation
                                        (Predicate "q" [Variable "z", Variable "u"])
                                        And
                                        (Predicate "p" [Variable "v", Variable "v"])
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ),
    test "2" (\_ ->
        Expect.equal 
        (Quantification ForAll "x"
            (Operation
                (Operation 
                    (Predicate "p" [Variable "x", Variable "y"])
                    Or
                    (Predicate "q" [Variable "z", Variable "u"])
                )
            And
                (Operation
                    (Predicate "p" [Variable "x", Variable "y"])
                    Or
                    (Operation 
                        (Negation (Predicate "p" [Variable "v", Variable "v"]))
                        Or
                        (Predicate "p" [Variable "z", Variable "x"])
                    )
                )
            )
        )
        (pNFtoCNF 
            (Quantification ForAll "x"
                (Operation 
                    (Predicate "p" [Variable "x", Variable "y"])
                    Or
                    (Operation
                        (Predicate "q" [Variable "z", Variable "u"])
                        And
                        (Operation
                            (Negation (Predicate "p" [Variable "v", Variable "v"]))
                            Or
                            (Predicate "p" [Variable "z", Variable "x"])
                        )
                    )
                )
            )
        )
    )    
    ]

toDisjunctSetTest : Test
toDisjunctSetTest = describe "toDisjunctSetTests" [
    test "simple" (\_ ->
        (Expect.equal True
            (DisjunctSet.equal (DisjunctSet.fromList
                [
                    Disjunct.fromList [
                        PositivePredicate "p" [Variable "x", Variable "y"],
                        PositivePredicate "q" [Variable "z", Variable "u"]
                    ],
                    Disjunct.fromList [
                        PositivePredicate "p" [Variable "x", Variable "y"],
                        NegativePredicate "p" [Variable "v", Variable "v"],
                        PositivePredicate "p" [Variable "z", Variable "x"]
                    ]
                ])
                (toDisjunctSet
                    (Quantification ForAll "x"
                        (Operation
                            (Operation 
                                (Predicate "p" [Variable "x", Variable "y"])
                                Or
                                (Predicate "q" [Variable "z", Variable "u"])
                            )
                        And
                            (Operation
                                (Predicate "p" [Variable "x", Variable "y"])
                                Or
                                (Operation 
                                    (Negation (Predicate "p" [Variable "v", Variable "v"]))
                                    Or
                                    (Predicate "p" [Variable "z", Variable "x"])
                                )
                            )
                        )
                    )
                )
            )
        )
    ),

    test "dublicates" (\_ ->
        (Expect.equal True
            (DisjunctSet.equal (DisjunctSet.fromList
                [
                    Disjunct.fromList [
                        PositivePredicate "p" [Variable "x", Variable "y"],
                        PositivePredicate "q" [Variable "z", Variable "u"]
                    ],
                    Disjunct.fromList [
                        NegativePredicate "p" [Variable "v", Variable "v"],
                        PositivePredicate "p" [Variable "z", Variable "x"]
                    ]
                ])
                (toDisjunctSet
                    (Quantification ForAll "x"
                        (Operation
                            (Operation 
                                (Predicate "p" [Variable "x", Variable "y"])
                                Or
                                (Predicate "q" [Variable "z", Variable "u"])
                            )
                        And
                            (Operation
                                (Operation 
                                    (Negation (Predicate "p" [Variable "v", Variable "v"]))
                                    Or
                                    (Predicate "p" [Variable "z", Variable "x"])
                                )
                                Or
                                (Operation 
                                    (Negation (Predicate "p" [Variable "v", Variable "v"]))
                                    Or
                                    (Predicate "p" [Variable "z", Variable "x"])
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    ]
