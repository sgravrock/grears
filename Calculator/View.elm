module Calculator.View exposing (view)

import Html exposing (Html, div)
import Array
import Select

import Calculator.Types exposing (Model, Msg(..))
import Calculator.State exposing (validateModel)
import Calculator.Form
import Calculator.ResultsView


view : (Msg -> a) -> Model -> Html a
view wrapMsg model =
  div []
    [ Calculator.Form.view wrapMsg model
    , maybeResultsView model
    ]

    
maybeResultsView : Model -> Html a
maybeResultsView model =
  case validateModel model of
    Just validModel -> Calculator.ResultsView.view validModel
    Nothing -> div [] []
