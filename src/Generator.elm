module Generator exposing (..)

import Set exposing (Set)

type alias Generator = {
        stateVars: List String,
        stateConsts: List String,
        stateFuncs: List String,
        forbiddenVars: Set String,
        forbiddenConsts: Set String,
        forbiddenFuncs: Set String
    }
createGenerator : Set String -> Set String -> Set String -> Generator
createGenerator fc ff fv = {
    forbiddenConsts = fc,
    forbiddenFuncs = ff,
    forbiddenVars = fv,
    stateConsts = ["a", "b", "c", "d", "e"],
    stateFuncs = ["f", "g", "h", "k", "l", "i", "j"],
    stateVars = ["x", "y", "z", "u", "v", "t", "w"]
    }

getConst : Generator -> (String, Generator)
getConst generator =
    let 
        const = Maybe.withDefault "a" (List.head generator.stateConsts)
        newState = Maybe.withDefault [] (List.tail generator.stateConsts)
        newConst = 
            let lastLetter = Maybe.withDefault '0' <| Maybe.map Tuple.first <| String.uncons <| String.right 1 const
            in
                if (Char.isAlpha lastLetter) || (lastLetter == '9') then const ++ "0"
                else (String.dropRight 1 const) ++ String.fromChar (Char.fromCode (Char.toCode lastLetter + 1))
        newGenerator = {generator | stateConsts = List.append newState [newConst]}
    in
    if Set.member const generator.forbiddenConsts then 
        getConst newGenerator
    else (const, newGenerator)

getFunc : Generator -> (String, Generator)
getFunc generator =
    let 
        func = Maybe.withDefault "f" (List.head generator.stateFuncs)
        newState = Maybe.withDefault [] (List.tail generator.stateFuncs)
        newFunc = 
            let lastLetter = Maybe.withDefault '0' <| Maybe.map Tuple.first <| String.uncons <| String.right 1 func
            in
                if (Char.isAlpha lastLetter) || (lastLetter == '9') then func ++ "0"
                else (String.dropRight 1 func) ++ String.fromChar (Char.fromCode (Char.toCode lastLetter + 1))
        newGenerator = {generator | stateFuncs = List.append newState [newFunc]}
    in
    if Set.member func generator.forbiddenFuncs then 
        getFunc newGenerator
    else (func, newGenerator)

getVar : Generator -> (String, Generator)
getVar generator =
    let 
        var = Maybe.withDefault "x" (List.head generator.stateVars)
        newState = Maybe.withDefault [] (List.tail generator.stateVars)
        newVar = 
            let lastLetter = Maybe.withDefault '0' <| Maybe.map Tuple.first <| String.uncons <| String.right 1 var
            in
                if (Char.isAlpha lastLetter) || (lastLetter == '9') then var ++ "0"
                else (String.dropRight 1 var) ++ String.fromChar (Char.fromCode (Char.toCode lastLetter + 1))
        newGenerator = {generator | stateVars = List.append newState [newVar]}
    in
    if Set.member var generator.forbiddenVars then 
        getVar newGenerator
    else (var, newGenerator)
