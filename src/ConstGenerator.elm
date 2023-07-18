module ConstGenerator exposing (..)

type alias Generator = {
        state: List String,
        forbidden: List String
    }
createGenerator : List String -> Generator
createGenerator forbidden = {forbidden = forbidden, state = ["a", "b", "c", "d"]}

getConst : Generator -> (String, Generator)
getConst generator =
    let 
        const = Maybe.withDefault "e" (List.head generator.state)
        newState = Maybe.withDefault [] (List.tail generator.state)
        newConst = 
            let lastLetter = Maybe.withDefault '0' <| Maybe.map Tuple.first <| String.uncons <| String.right 1 const
            in
                if (Char.isAlpha lastLetter) || (lastLetter == '9') then const ++ "0"
                else (String.dropRight 1 const) ++ String.fromChar (Char.fromCode (Char.toCode lastLetter + 1))
        newGenerator = {generator | state = List.append newState [newConst]}
    in
    if List.member const generator.forbidden then 
        getConst newGenerator
    else (const, newGenerator )

getNConsts : Int -> Generator -> (List String, Generator)
getNConsts n generator =
    case n of
        0 -> ([], generator)
        _ ->
            let 
                (list, newGenerator) = getNConsts (n - 1) generator
                (const, newGenerator1) = getConst newGenerator
            in
                (List.append list [const], newGenerator1)
