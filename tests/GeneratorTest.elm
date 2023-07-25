module GeneratorTest exposing (..)

import Generator exposing (..)

import Expect exposing (..)
import Test exposing (Test, test, describe)
import Set

tests : Test
tests =
    let
        generator = createGenerator
            (Set.fromList ["a", "b"])
            (Set.fromList ["f", "g"])
            (Set.fromList ["x", "y"])
    in
    describe "Generator tests" [
        test "generate consts" (\_ ->
            let expectedList = ["c", "d", "e", "a0", "b0", "c0", "d0", "e0", "a1", "b1", "c1", "d1", "e1"]
            in
                Expect.equalLists
                    expectedList
                    (List.foldr (\ _ (gen, list) ->
                        let (c, newGen) = getConst gen
                        in (newGen, List.append list [c]))
                    (generator, [])
                    (List.range 1 (List.length expectedList)) |> Tuple.second )
        ),
        test "generate funcs" (\_ ->
            let expectedList = ["h", "i", "j", "f0", "g0", "h0", "i0", "j0", "f1", "g1", "h1", "i1", "j1"]
            in
                Expect.equalLists
                    expectedList
                    (List.foldr (\ _ (gen, list) ->
                        let (f, newGen) = getFunc gen
                        in (newGen, List.append list [f]))
                    (generator, [])
                    (List.range 1 (List.length expectedList)) |> Tuple.second )
        ),
        test "generate vars" (\_ ->
            let expectedList = ["z", "u", "v", "x0", "y0", "z0", "u0", "v0", "x1", "y1", "z1", "u1", "v1"]
            in
                Expect.equalLists
                    expectedList
                    (List.foldr (\ _ (gen, list) ->
                        let (x, newGen) = getVar gen
                        in (newGen, List.append list [x]))
                    (generator, [])
                    (List.range 1 (List.length expectedList)) |> Tuple.second )
        )
    ]
