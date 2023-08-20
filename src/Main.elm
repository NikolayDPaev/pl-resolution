module Main exposing (..)

import Browser
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)

import DisjunctSet
import Disjunct
import Language exposing (Literal(..))
import Language exposing (Term(..))
import Search exposing (resolutionMethod)
import ResolutionStep exposing (logEntryToString)

type alias Model =
    { buttonText : String
    , displayText : String
    }

initialModel : Model
initialModel =
    { buttonText = "Click me!"
    , displayText = ""
    }

-- UPDATE

type Msg
    = ButtonClicked

update : Msg -> Model -> Model
update msg model =
    case msg of
        ButtonClicked ->
            { model | displayText = algorithm }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ButtonClicked ] [ text model.buttonText ]
        , div [] [ text model.displayText ]
        ]

-- MAIN

main =
    Browser.sandbox { init = initialModel, update = update, view = view }

algorithm : String
algorithm = 
    let
        disjuncts = DisjunctSet.fromList [
            Disjunct.fromList [NegativePredicate "p" [Constant "a", Variable "y"], NegativePredicate "p" [Variable "z", Variable "y"], NegativePredicate "p" [Variable "y", Variable "z"]],
            Disjunct.fromList [PositivePredicate "p" [Constant "a", Variable "y"], PositivePredicate "p" [Function "f" [Variable "y"], Variable "y"]],
            Disjunct.fromList [PositivePredicate "p" [Constant "a", Variable "y"], PositivePredicate "p" [Variable "y", Function "f" [Variable "y"]]]
            ]
    in
    resolutionMethod disjuncts
        |> Maybe.map (List.map logEntryToString)
        |> Maybe.withDefault []
        |> List.foldl (\ line acc -> String.concat [acc, " ", line]) ""
    