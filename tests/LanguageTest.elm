module LanguageTest exposing (..)

import Language exposing (..)

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
            (printFormula (ForAll "x" (Exists "y" (Operation (ForAll "z" (Predicate "p" [Variable "z", Variable "y"])) Impl (Predicate "p" [Variable "z", Variable "x"])))))
        )
    ]

eliminateImplandEqvTest : Test
eliminateImplandEqvTest =
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

