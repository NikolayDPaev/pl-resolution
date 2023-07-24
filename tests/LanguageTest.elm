module LanguageTest exposing (..)

import Language exposing (..)

import Set
import Expect exposing (..)
import Test exposing (Test, test, describe)

printTest : Test
printTest =
    describe "Printing tests" [
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
    test "test" (\_ ->
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
moveNegationsTest =
    describe "tests" [
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
