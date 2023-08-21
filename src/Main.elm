module Main exposing (..)

import Parser
import Language exposing (..)
import Transformations exposing (..)
import DisjunctSet exposing (DisjunctSet)
import Search exposing (..)
import ResolutionStep exposing (logEntryToString)
import ListHelperFunctions exposing (listOfMaybesToMaybeList)

import Browser
import Html exposing (Html, div, input, button, text, span)
import Html.Attributes exposing (placeholder, class, value)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)

type alias Model =
    { variables : String
    , constants : String
    , predicates : String
    , functions : String
    , language : Language
    , formulaStrings : List (String, String)
    , formulas : List (Maybe Formula)
    , transformedFormulasText : List (List String)
    , disjunctSet : DisjunctSet
    , transformationResult : String
    , resolutionSteps : List String
    }

init : Model
init =
    { variables = ""
    , constants = ""
    , predicates = ""
    , functions = ""
    , language = Language.empty
    , formulaStrings = []
    , formulas = []
    , transformedFormulasText = []
    , disjunctSet = DisjunctSet.empty
    , transformationResult = ""
    , resolutionSteps = []
    }

type Msg
    = UpdateConstants String
    | UpdateVariables String
    | UpdateFunctions String
    | UpdatePredicates String
    | AddFormula
    | UpdateFormula Int String
    | StartTransformations
    | StartResolution

update : Msg -> Model -> Model
update msg model =
    let
        modelLanguage = model.language
        parseLanguageSet : String -> Set String
        parseLanguageSet str =
            str 
                |> String.split ","
                |> List.map String.trim
                |> Set.fromList

        updateAtIndex : Int -> a -> List a -> List a
        updateAtIndex index value =
            List.indexedMap (\ i el -> if i == index then value else el)

        updateAllFormulas : Model -> Model
        updateAllFormulas m =
            m.formulaStrings
            |> List.map Tuple.first
            |> List.indexedMap Tuple.pair
            |> List.foldl
                (\ (index, formula) newModel -> 
                    update (UpdateFormula index formula) newModel)
                m
    in
    case msg of
        UpdateConstants value ->
            { model | constants = value, language = {modelLanguage | consts = parseLanguageSet value}} |> updateAllFormulas

        UpdateVariables value ->
            { model | variables = value, language = {modelLanguage | vars = parseLanguageSet value}} |> updateAllFormulas

        UpdateFunctions value ->
            { model | functions = value, language = {modelLanguage | funcs = parseLanguageSet value}} |> updateAllFormulas

        UpdatePredicates value ->
            { model | predicates = value, language = {modelLanguage | preds = parseLanguageSet value}} |> updateAllFormulas

        AddFormula -> { 
                model |
                    formulas = model.formulas ++ [Nothing],
                    formulaStrings = model.formulaStrings ++ [("", "")]
                }

        UpdateFormula index formulaString ->
            case Parser.parse model.language formulaString of
                Ok formula -> { 
                    model | formulas = updateAtIndex index (Just formula) model.formulas, 
                            formulaStrings = updateAtIndex index (formulaString, "") model.formulaStrings
                    }
                Err error -> {
                    model | formulaStrings = updateAtIndex index (formulaString, Parser.errToString error) model.formulaStrings
                    }
                
        StartTransformations ->
            let
                transform : Formula -> Language -> (DisjunctSet, Language, List String)
                transform f l =
                    let
                        line1 = ("Original formula: ", printFormula f)
                        f1 = eliminateImplAndEqv f
                        line2 = ("Elimination of implications and equivalences: ", printFormula f1)
                        f2 = moveNegations f1
                        line3 = ("Moving negations inside: ", printFormula f2)
                        (f3, l1) = skolemization l f2
                        line4 = ("Skolemization: ", printFormula f3)
                        (f4, l2) = toPNF l1 f3
                        line5 = ("Converting to Prenex Normal Form: ", printFormula f4)
                        f5 = pNFtoCNF f4
                        line6 = ("Converting to Conjunctive Normal Form: ", printFormula f5)
                        lString = printLanguage l
                        l2String = printLanguage l2
                        ds = toDisjunctSet f5
                        line7 = " Formula disjunct set: " ++ DisjunctSet.toString ds
                        removeDuplicatesBySecondElement : List (String, String) -> List (String, String)
                        removeDuplicatesBySecondElement dupList =
                            let 
                                member : String -> List (String, String) -> Bool
                                member f11 listToSearch =
                                    case listToSearch of
                                        [] -> False
                                        (_, f21) :: ls -> if f11 == f21 then True else member f11 ls
                            in
                            List.foldl (\item acc -> if member (Tuple.second item) acc then acc else item :: acc) [] dupList
                    in
                    (ds, l2, 
                        ([line1, line2, line3, line4, line5, line5, line6]
                            |> removeDuplicatesBySecondElement
                            |> List.reverse
                            |> List.map (\ (stepName, formula) -> stepName ++ formula))
                        ++ line7 :: if lString /= l2String then [" Language is updated to " ++ l2String] else []
                    )
                
                clearedModel = { model | transformedFormulasText = [["Transformations: "]], transformationResult = "", language = Language.empty }
                (newModel, finalDisjunctSet) =
                    case listOfMaybesToMaybeList model.formulas of
                        Just formulas ->
                            List.foldr (\ f (oldModel, dss) ->
                                let
                                    (ds, newL, lines) = transform f oldModel.language
                                in
                                ({oldModel | transformedFormulasText = lines :: oldModel.transformedFormulasText,
                                             language = newL
                                }, DisjunctSet.union dss ds)) (clearedModel, DisjunctSet.empty) formulas
                        Nothing -> (clearedModel, DisjunctSet.empty)
            in
            {newModel | transformationResult = "Final Disjunct set: " ++ DisjunctSet.toString finalDisjunctSet
                      , disjunctSet = finalDisjunctSet}

        StartResolution ->
            -- must be async
            let
                logs = Maybe.withDefault [] (resolutionMethod model.disjunctSet)
                steps = List.map logEntryToString logs
            in
            { model | resolutionSteps = "Resolution" :: steps }

view : Model -> Html Msg
view model =
    div []
        [ div [ class "section" ]
            [ div [ class "label" ] [ text "Language" ]
            , input [ placeholder "Variables", value model.variables, onInput UpdateVariables ] []
            , input [ placeholder "Constants", value model.constants, onInput UpdateConstants ] []
            , input [ placeholder "Predicates", value model.predicates, onInput UpdatePredicates ] []
            , input [ placeholder "Functions", value model.functions, onInput UpdateFunctions ] []
            ]
        , div [ class "section" ]
            [ div [ class "label" ] [ text "Formulas:" ]
            , div [] 
                (List.concat (List.indexedMap
                    (\ index (formula, error) -> [
                        div [] [
                            input [ placeholder ("Formula " ++ (String.fromInt (index + 1))), value formula, onInput (UpdateFormula index) ] [],
                            span [class "error"] [text error]
                        ]
                    ])
                    model.formulaStrings
                ))
            , button [ onClick AddFormula ] [ text "Add Formula" ]
            ]
        , button [ onClick StartTransformations ] [ text "Make Transformations" ]
        , div [ class "result" ] 
            [ div []
                (List.concat (List.indexedMap
                    (\ index formulaLines -> [
                        div [class "label"] [text ("Formula #" ++ (String.fromInt index))], 
                        div [] (List.map (\ step -> div [class "result"] [text step]) formulaLines)
                    ])
                    model.transformedFormulasText
                ))
            , div [] [text model.transformationResult]
            ]
        , button [ onClick StartResolution ] [ text "Apply Resolution" ]
        , div [ class "result" ]
            [ div [] (List.map (\ step -> div [class "result"] [text step]) model.resolutionSteps)]
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }

    