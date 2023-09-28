module Main exposing (..)

import Parser
import Language exposing (..)
import Transformations exposing (..)
import DisjunctSet exposing (DisjunctSet)
import Search exposing (..)

import Browser
import Html exposing (Html, div, input, button, text, span, ol, li)
import Html.Attributes exposing (placeholder, class, value)
import Html.Events exposing (onClick, onInput, onFocus)
import Set exposing (Set)
import Html.Attributes exposing (type_)

type alias Model =
    { variables : String
    , predicates : String
    , functions : String
    , constants : String
    , inputLanguage : Language
    , language : Language
    , formulaStrings : List (String, String)
    , focusOnFormulaNumber : Maybe Int
    , formulas : List (Maybe Formula)
    , transformedFormulasText : List (List (String, String))
    , disjunctSet : DisjunctSet
    , transformationResult : (String, String)
    , maxDepth : String
    , resolutionSteps : List (String, String)
    , resolutionError : Maybe String
    }

init : Model
init =
    { variables = ""
    , predicates = ""
    , functions = ""
    , constants = ""
    , inputLanguage = Language.empty
    , language = Language.empty
    , formulaStrings = [("", "")]
    , focusOnFormulaNumber = Nothing
    , formulas = [Nothing]
    , transformedFormulasText = []
    , disjunctSet = DisjunctSet.empty
    , transformationResult = ("", "")
    , maxDepth = "10"
    , resolutionSteps = []
    , resolutionError = Nothing
    }

type Msg
    = UpdateVariables String
    | UpdatePredicates String
    | UpdateConstants String
    | UpdateFunctions String
    | AddFormula
    | NegationOfLast
    | AddSymbol Char
    | FocusChanged (Maybe Int)
    | UpdateFormula Int String
    | StartTransformations
    | UpdateMaxDepth String
    | StartResolution

update : Msg -> Model -> Model
update msg model =
    let
        modelLanguage = model.inputLanguage
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
        UpdateVariables value ->
            { model | variables = value, inputLanguage = {modelLanguage | vars = parseLanguageSet value}} |> updateAllFormulas

        UpdatePredicates value ->
            { model | predicates = value, inputLanguage = {modelLanguage | preds = parseLanguageSet value}} |> updateAllFormulas

        UpdateFunctions value ->
            { model | functions = value, inputLanguage = {modelLanguage | funcs = parseLanguageSet value}} |> updateAllFormulas

        UpdateConstants value ->
            { model | constants = value, inputLanguage = {modelLanguage | consts = parseLanguageSet value}} |> updateAllFormulas

        AddFormula -> { 
            model |
                formulas = model.formulas ++ [Nothing],
                formulaStrings = model.formulaStrings ++ [("", "")]
            }

        NegationOfLast ->
            let
                prependToLastNonEmpty : List (String, String) -> List (String, String)
                prependToLastNonEmpty strings =
                    let
                        prependToFirstNonEmpty str =
                            case str of
                                [] -> []
                                (head, err) :: tail ->
                                    if String.isEmpty head then
                                        (head, err) :: prependToFirstNonEmpty tail
                                    else
                                        ("¬" ++ head, err) :: tail
                    in
                    List.reverse (prependToFirstNonEmpty (List.reverse strings))
            in
            { model | formulaStrings = prependToLastNonEmpty model.formulaStrings }
                    |> updateAllFormulas

        AddSymbol symbol ->
            case model.focusOnFormulaNumber of
                Just index -> 
                    { model | formulaStrings = List.indexedMap 
                            (\ i el -> 
                                if i == index then 
                                    Tuple.mapFirst (\ x -> x ++ String.fromChar symbol) el
                                else el)
                            model.formulaStrings
                    }
                Nothing -> model

        FocusChanged maybeInt -> 
            { model | focusOnFormulaNumber = maybeInt}

        UpdateFormula index formulaString ->
            if String.isEmpty formulaString then { 
                    model | formulas = updateAtIndex index Nothing model.formulas, 
                            formulaStrings = updateAtIndex index (formulaString, "") model.formulaStrings
                    }
            else 
            case Parser.parse model.inputLanguage formulaString of
                Ok formula -> { 
                    model | formulas = updateAtIndex index (Just formula) model.formulas, 
                            formulaStrings = updateAtIndex index (formulaString, "") model.formulaStrings
                    }
                Err error -> {
                    model | formulaStrings = updateAtIndex index (formulaString, Parser.errToString error) model.formulaStrings
                    }
                
        StartTransformations ->
            let
                transform : Formula -> Language -> (DisjunctSet, Language, List (String, String))
                transform f l =
                    let
                        line1 = ("Original formula: ",printFormula f)
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
                        line7 = ("Formula disjunct set:", DisjunctSet.toString ds)
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
                        )
                        ++ line7 :: if lString /= l2String then [(" Language is updated to ", l2String)] else []
                    )
                
                clearedModel = { model | transformedFormulasText = [], 
                                         transformationResult = ("", ""), 
                                         language = model.inputLanguage,
                                         disjunctSet = DisjunctSet.empty }
                filteredFormulas = List.filterMap identity model.formulas
                
                (newM, finalDisjunctSet) = List.foldl (\ f (oldModel, dss) ->
                    let
                        (ds, newL, lines) = transform f oldModel.language
                    in
                    (
                        { oldModel | transformedFormulasText = oldModel.transformedFormulasText ++ [lines], language = newL}
                        , DisjunctSet.union dss ds
                    )
                    ) (clearedModel, DisjunctSet.empty) filteredFormulas

            in {newM | transformationResult =
                        if List.isEmpty newM.transformedFormulasText 
                            then ("", "")
                        else ("Final Disjunct set:", DisjunctSet.toIndexedString finalDisjunctSet)
                     , disjunctSet = finalDisjunctSet}

        UpdateMaxDepth str ->
            {model | maxDepth = str}

        StartResolution ->
            if DisjunctSet.isEmpty model.disjunctSet then
                { model | resolutionSteps = []}
            else
                case (String.toInt model.maxDepth |> Maybe.andThen (\ num -> if num <= 0 then Nothing else Just num)) of
                    Nothing -> {model | maxDepth = "10"}
                    Just maxDepth -> 
                        case (resolutionMethod model.disjunctSet maxDepth) of
                            Just log -> {model | resolutionSteps = log, resolutionError = Nothing}
                            Nothing -> {
                                model | resolutionSteps = [], 
                                        resolutionError = Just ("Unable to solve in under " ++ model.maxDepth ++ " steps.")        
                                }

view : Model -> Html Msg
view model =
    div [ class "app" ] [   
        div [ class "language" ]
            [ div [ class "label" ] [ text "Language: " ]
            , input [ class "language-input", placeholder "Variables", value model.variables, onInput UpdateVariables ] []
            , input [ class "language-input", placeholder "Predicates", value model.predicates, onInput UpdatePredicates ] []
            , input [ class "language-input", placeholder "Functions", value model.functions, onInput UpdateFunctions ] []
            , input [ class "language-input", placeholder "Constants", value model.constants, onInput UpdateConstants ] []
            ]
        , div [ class "formulas-insert-container" ] [
                div [ class "formula-column" ]
                [ div [ class "label" ] [ text "Formulas:" ]
                , div [] 
                    (List.concat (List.indexedMap
                        (\ index (formula, error) -> [
                            div [] [
                                input [ class "formula-input"
                                    , placeholder ("Formula")
                                    , value formula
                                    , onInput (UpdateFormula index)
                                    , onFocus (FocusChanged (Just index)) ] [],
                                div [class "error"] [text error]
                            ]
                        ])
                        model.formulaStrings
                    ))
                , div [] [
                    button [ onClick AddFormula ] [ text "Add Formula" ]
                    , button [ onClick NegationOfLast ] [ text "Negation of last formula" ]
                    ]
                ]
            , div [ class "insert-column" ]
                [ div [ class "label" ] [ text "Insert special character:" ]
                , div [ class "insert-buttons" ] [
                    button [onClick (AddSymbol '∀') ] [ text "∀"]
                    , button [onClick (AddSymbol '∃') ] [ text "∃"]
                    , button [onClick (AddSymbol '&') ] [ text "&"]
                    , button [onClick (AddSymbol '∨') ] [ text "∨"]
                    , button [onClick (AddSymbol '⇔') ] [ text "⇔"]
                    , button [onClick (AddSymbol '⇒') ] [ text "⇒"]
                    , button [onClick (AddSymbol '¬') ] [ text "¬"]
                    ]
                ]
            ]
        , div [class "transformations"] [
              button [ onClick StartTransformations ] [ text "Make Transformations" ]
            , div [ class "transformations-body" ]
                [ div []
                    (List.indexedMap
                        (\ index formulaLines -> div [ class "formula-transformation" ] [
                            div [ class "label" ] [text ("Formula " ++ (String.fromInt (index + 1)))], 
                            div [] (List.map (\ (step, formula) -> div [ class "single-transformation" ] [
                                span [] [text step],
                                span [ class "formula" ] [text formula]
                            ]) formulaLines)
                        ])
                        model.transformedFormulasText
                    )
                , div [] [
                    span [] [text (Tuple.first model.transformationResult)],
                    span [ class "formula" ] [text (Tuple.second model.transformationResult)]]
                ]
            ]
        , div [class "resolution"] [
                div [class "max-steps"] [
                    div [ class "label" ] [ text "Max steps:" ]
                    , input [class "steps-input", type_ "number", value model.maxDepth, onInput UpdateMaxDepth] []
                ]
              , button [ onClick StartResolution ] [ text "Apply Resolution" ]
              , div [class "error"] [
                    text (Maybe.withDefault "" model.resolutionError)]
              , ol [class "resolution-result"]
                (List.map (\ (sub, step) -> li [class "resolution-step"] [
                    span [] [text sub],
                    span [ class "res-step" ] [text step]
                ]) model.resolutionSteps)
        ]
    ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
