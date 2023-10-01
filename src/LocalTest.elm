module LocalTest exposing (..)

import Set exposing (Set)
import Language exposing (Language, Formula)
import Parser exposing (parse, ParseError(..))
import ListHelperFunctions exposing (listOfResultToResultList)
import Transformations exposing (..)
import DisjunctSet exposing (DisjunctSet)
import Search exposing (resolutionMethod)

type Error
    = ParseError ParseError
    | ResolutionError

transformations : String -> String -> String -> String -> List String -> Result Error DisjunctSet
transformations vars preds funcs consts formulaStrings =
    let
        parseLanguageSet : String -> Set String
        parseLanguageSet str =
            str 
            |> String.split ","
            |> List.map String.trim
            |> Set.fromList

        transform : Formula -> Language -> (DisjunctSet, Language)
        transform f l =
            let
                f1 = eliminateImplAndEqv f
                f2 = moveNegations f1
                (f3, l1) = skolemization l f2
                (f4, l2) = toPNF l1 f3
                f5 = pNFtoCNF f4
                ds = toDisjunctSet f5
            in
            (ds, l2)

        toSingleDisjunctSet : Language -> List Formula -> (Language, DisjunctSet)
        toSingleDisjunctSet l formulas = List.foldl (\ f (language, dss) ->
                let
                    (ds, newL) = transform f language
                in
                (newL, DisjunctSet.union dss ds)
            ) (l, DisjunctSet.empty) formulas


        lang = {vars = parseLanguageSet vars, consts = parseLanguageSet consts, preds = parseLanguageSet preds, funcs = parseLanguageSet funcs}
        resultListFormulas = listOfResultToResultList (List.map (parse lang) formulaStrings)
    in
    resultListFormulas
        |> Result.mapError ParseError
        |> Result.map (toSingleDisjunctSet lang)
        |> Result.map Tuple.second

resolution : Int -> DisjunctSet -> Result Error (List (String, String))
resolution depth ds = 
    Result.fromMaybe ResolutionError (resolutionMethod ds depth)
 