module ParserTest exposing (..)

import Parser exposing (ParseError(..))
import Language exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)

lang : Language
lang = {vars = ["x", "y", "z"], consts = ["a", "b", "c"], preds = ["p", "q", "r"], funcs = ["f", "g", "h"]}

propLogicTests : Test
propLogicTests =
    describe "propositional logic" [
        test "And and Or" (\_ ->
            Expect.equal
            (Ok (Operation 
                (Operation 
                    (Predicate "p" [Constant "c", Variable "x"])
                    Or 
                    (Negation (Predicate "q" [Function "f" [Constant "c"]])))
                And
                (Predicate "q" [Function "g" [Variable "y", Function "f" [Constant "c", Variable "x"]]])))
            (Parser.parse lang "(p(c, x) ∨ ¬q(f(c))) & q(g(y, f(c, x)))")
        ),
        test "Impl and Eqv" (\_ ->
            Expect.equal
            (Ok (Operation 
                (Operation 
                    (Predicate "p" [Constant "a", Constant "b"])
                    Eqv 
                    (Negation (Predicate "q" [Function "f" [Constant "c"]])))
                Impl 
                 (Negation (Predicate "q" [Function "g" [Variable "y", Function "f" [Constant "c", Variable "x"]]]))))
            (Parser.parse lang "(p(a, b) ⇔ ¬q(f(c))) ⇒ ¬q (g(y, f(c, x)))")
        ),
        test "predicate in predicate" (\_ ->
            Expect.equal
            (Err (PredicateInTerm "p"))
            (Parser.parse lang "(p(a, p(b)) ⇔ ¬q(f(c))) ⇒ ¬q (g(y, f(c, x)))")
        ),
        test "predicate in function" (\_ ->
            Expect.equal
            (Err (PredicateInTerm "p"))
            (Parser.parse lang "(p(a, b) ⇔ ¬q(f(p))) ⇒ ¬q (g(y, f(c, x)))")
        )

    ]

predTests : Test
predTests = 
    describe "Predicate logic tests" [
        test "quantors" (\_ ->
            Expect.equal
            (Ok (ForAll "x" (Exists "y" (Operation (ForAll "z" (Predicate "p" [Variable "z", Variable "y"])) Impl (Predicate "p" [Variable "z", Variable "x"])))))
            (Parser.parse lang "∀x∃y(∀z p(z, y)) ⇒ p(z, x)")
        ),
        test "Expected a variable" (\_ ->
            Expect.equal
            (Err (ExpectedVariable "Const: a"))
            (Parser.parse lang "∀a p(x, a)")
        )
    ]
