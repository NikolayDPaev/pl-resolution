module IntegrationTest exposing (..)

import LocalTest exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)

vars : String
vars = "x, y, z, u, v, w, t"

preds : String
preds = "p, q, r, s"

funcs : String
funcs = "f"

correctStepsCountTest : Test
correctStepsCountTest =
    describe "testing solving in under the correct amount of steps" [
        test "0" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃z∀x(∀y p(f(x), y) ∨ q(z, f(x)))",
                "∃z∀y(q(y, f(f(y))) ⇒ ∀x p(x, f(z)))", 
                "∀x∀y(p(x, f(y)) ⇒ (p(f(y), x) ∨ ¬∃y∃x p(x, y)))",
                "¬∃y(∃x p(f(x), y) & ∃x p(y, x))"
            ]
                |> Result.andThen (resolution 4)
                |> Result.map List.length)
        ),
        test "1" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀y(∀z(¬s(z) ⇒ ¬p(y, z)) ⇒ r(y))",
                "∃z∀y(s(y) ∨ q(z, y))",
                "¬∃y∀z(¬r(z) ⇒ ∃t(p(z, t) & q(y, t)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "2" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀y∃z(¬r(z) & ∀t(p(z, t) ⇒ ¬q(y, t)))",
                "∀y(∀z(¬s(z) ⇒ ¬p(y, z)) ⇒ r(y))",
                "¬∀z∃y(¬s(y) & ¬q(z, y))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "3" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃z∀y(s(y) ∨ q(z, y))",
                "∀y∃z(¬r(z) & ∀t(p(z, t) ⇒ ¬q(y, t)))",
                "¬∃y(∀z(¬s(z) ⇒ ¬p(y, z)) & ¬r(y))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "4" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀z(∀t(¬s(t) ⇒ p(t, z)) ⇒ r(z))",
                "∃t∀z(s(z) ∨ ¬q(z, t))",
                "¬∃z∀t(¬r(t) ⇒ ∃u(¬p(u, t) & ¬q(u, z)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "5" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀z∃t(¬r(t) & ∀u(¬p(u, t) ⇒ q(u, z)))",
                "∀z(∀t(¬s(t) ⇒ p(t, z)) ⇒ r(z))",
                "¬∀t∃z(¬s(z) & q(z, t))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "6" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃t∀z(s(z) ∨ ¬q(z, t))",
                "∀z∃t(¬r(t) & ∀u(¬p(u, t) ⇒ q(u, z)))",
                "¬∃z(∀t(¬s(t) ⇒ p(t, z)) & ¬r(z))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "7" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀t(∀u(¬p(u) ⇒ ¬q(t, u)) ⇒ s(t))",
                "∃u∀t(p(t) ∨ ¬r(u, t))",
                "¬∃t∀u(¬s(u) ⇒ ∃v(q(u, v) & ¬r(t, v)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "8" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀t∃u(¬s(u) & ∀v(q(u, v) ⇒ r(t, v)))",
                "∀t(∀u(¬p(u) ⇒ ¬q(t, u)) ⇒ s(t))",
                "¬∀u∃t(¬p(t) & r(u, t))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "9" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃u∀t(p(t) ∨ ¬r(u, t))",
                "∀t∃u(¬s(u) & ∀v(q(u, v) ⇒ r(t, v)))",
                "¬∃t(∀u(¬p(u) ⇒ ¬q(t, u)) & ¬s(t))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "10" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀u(∀v(¬p(v) ⇒ q(v, u)) ⇒ ¬s(u))",
                "∃v∀u(p(u) ∨ r(u, v))",
                "¬∃u∀v(s(v) ⇒ ∃w(¬q(w, v) & r(w, u)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "11" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀u∃v(s(v) & ∀w(¬q(w, v) ⇒ ¬r(w, u)))",
                "∀u(∀v(¬p(v) ⇒ q(v, u)) ⇒ ¬s(u))",
                "¬∀v∃u(¬p(u) & ¬r(u, v))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "12" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃v∀u(p(u) ∨ r(u, v))",
                "∀u∃v(s(v) & ∀w(¬q(w, v) ⇒ ¬r(w, u)))",
                "¬∃u(∀v(¬p(v) ⇒ q(v, u)) & s(u))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "13" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀v(∀w(¬p(w) ⇒ ¬q(v, w)) ⇒ ¬s(v))",
                "∃w∀v(p(v) ∨ r(w, v))",
                "¬∃v∀w(s(w) ⇒ ∃x(q(w, x) & r(v, x)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "14" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀v∃w(s(w) & ∀x(q(w, x) ⇒ ¬r(v, x)))",
                "∀v(∀w(¬p(w) ⇒ ¬q(v, w)) ⇒ ¬s(v))",
                "¬∀w∃v(¬p(v) & ¬r(w, v))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "15" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃w∀v(p(v) ∨ r(w, v))",
                "∀v∃w(s(w) & ∀x(q(w, x) ⇒ ¬r(v, x)))",
                "¬∃v(∀w(¬p(w) ⇒ ¬q(v, w)) & s(v))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "16" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀w(∀x(¬q(x) ⇒ r(x, w)) ⇒ ¬p(w))",
                "∃x∀w(q(w) ∨ ¬s(w, x))",
                "¬∃w∀x(p(x) ⇒ ∃y(¬r(y, x) & ¬s(y, w)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "17" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀w∃x(p(x) & ∀y(¬r(y, x) ⇒ s(y, w)))",
                "∀w(∀x(¬q(x) ⇒ r(x, w)) ⇒ ¬p(w))",
                "¬∀x∃w(¬q(w) & s(w, x))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "18" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃x∀w(q(w) ∨ ¬s(w, x))",
                "∀w∃x(p(x) & ∀y(¬r(y, x) ⇒ s(y, w)))",
                "¬∃w(∀x(¬q(x) ⇒ r(x, w)) & p(w))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "19" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀x(∀y(¬q(y) ⇒ ¬r(x, y)) ⇒ ¬p(x))",
                "∃y∀x(q(x) ∨ ¬s(y, x))",
                "¬∃x∀y(p(y) ⇒ ∃z(r(y, z) & ¬s(x, z)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "20" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀x∃y(p(y) & ∀z(r(y, z) ⇒ s(x, z)))",
                "∀x(∀y(¬q(y) ⇒ ¬r(x, y)) ⇒ ¬p(x))",
                "¬∀y∃x(¬q(x) & s(y, x))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "21" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∃y∀x(q(x) ∨ ¬s(y, x))",
                "∀x∃y(p(y) & ∀z(r(y, z) ⇒ s(x, z)))",
                "¬∃x(∀y(¬q(y) ⇒ ¬r(x, y)) & p(x))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        ),
        test "22" (\_ ->
            Expect.equal
            (Ok 4)
            (transformations vars preds funcs "" [
                "∀y(∀z(q(z) ⇒ r(z, y)) ⇒ p(y))",
                "∃z∀y(¬q(y) ∨ s(y, z))",
                "¬∃y∀z(¬p(z) ⇒ ∃t(¬r(t, z) & s(t, y)))"
            ]
            |> Result.andThen (resolution 4)
            |> Result.map List.length)
        )
    ]

-- transformations "z, x, y" "p, q" "f" "" ["∃z∀x(∀y p(f(x), y) ∨ q(z, f(x)))", "∃z∀y(q(y, f(f(y))) ⇒ ∀x p(x, f(z)))", "∀x∀y(p(x, f(y)) ⇒ (p(f(y), x) ∨ ¬∃y∃x p(x, y)))", "¬∃y(∃x p(f(x), y) & ∃x p(y, x))"] |> Result.andThen (resolution 5)
-- fixed

-- transformations "x, y, z" "p, q, r, s" "" "" ["∃y∀x(q(x) ∨ ¬s(y, x))", "∀x∃y(p(y) & ∀z(r(y, z) ⇒ s(x, z)))", "¬∃x(∀y(¬q(y) ⇒ ¬r(x, y)) & p(x))"] |> Result.andThen (resolution 5)
-- disjuncts = DisjunctSet.fromList[
--         (Disjunct.fromList [PositivePredicate "p" [Function "f" [Variable "x"]]]),
--         (Disjunct.fromList [PositivePredicate "q" [Variable "x"], NegativePredicate "s" [Constant "a",Variable "x"]]),
--         (Disjunct.fromList [PositivePredicate "r" [Variable "x", Function "g" [Variable "x"]], NegativePredicate "p" [Variable "x"]]),
--         (Disjunct.fromList [PositivePredicate "s" [Variable "x", Variable "z"], NegativePredicate "r" [Function "f" [Variable "x"], Variable "z"]]),
--         (Disjunct.fromList [NegativePredicate "p" [Variable "x"], NegativePredicate "q" [Function "g" [Variable "x"]]])
--     ]
-- failure to start
