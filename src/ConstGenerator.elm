module ConstGenerator exposing (..)

type alias Generator = List String

createGenerator : Generator
createGenerator = ["a", "b", "c", "d"]

getConst : Generator -> (String, Generator)
getConst generator =
    let 
        const = Maybe.withDefault "e" (List.head generator)
        newGenerator = Maybe.withDefault [] (List.tail generator)
        newConst = 
            let lastLetter = Maybe.withDefault '0' <| Maybe.map Tuple.first <| String.uncons <| String.right 1 const
            in
                if (Char.isAlpha lastLetter) || (lastLetter == '9') then const ++ "0"
                else (String.dropRight 1 const) ++ String.fromChar (Char.fromCode (Char.toCode lastLetter + 1))
    in
    (const, List.append newGenerator [newConst])

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
