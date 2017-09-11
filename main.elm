import Html exposing (Html, div, button, text)
import Html.Attributes exposing (rel, href, class)
import Html.Events exposing (onClick)
import Array

import Calculator.Types
import Calculator.State
import Calculator.View

type alias Model = Array.Array Calculator.Types.Model
type Msg
  = UpdateCalculator Int Calculator.Types.Msg
  | AddCalculator

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

model : Model
model = Array.fromList [Calculator.State.model]

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddCalculator -> Array.push Calculator.State.model model
    UpdateCalculator targetIndex targetMsg ->
      case Array.get targetIndex model of
        Nothing -> model
        Just orig ->
          let
            updated = Calculator.State.update targetMsg orig
          in
            Array.set targetIndex updated model

view : Model -> Html Msg
view model =
  div []
    [
      Html.node "link" [ rel "stylesheet", href "grears.css" ] []
    , button [onClick AddCalculator] [text "Add another calculator"]
    , div [class "calculators"]
      (Array.toList (Array.indexedMap viewForCalc model))
    ]

viewForCalc : Int -> Calculator.Types.Model -> Html Msg
viewForCalc i model =
  let
    subview = Calculator.View.view model
  in
    Html.map (UpdateCalculator i) subview
