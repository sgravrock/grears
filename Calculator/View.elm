module Calculator.View exposing (view)

import Html exposing (Html, div)
import Array
import Select

import Calculator.Types exposing (Model, Msg(..))
import Calculator.State exposing (validateModel)
import Calculator.Form
import Calculator.ResultsView


view : Model -> Html Msg
view model =
  div []
    [ Calculator.Form.view model
    , maybeResultsView model
    ]

    
maybeResultsView : Model -> Html Msg
maybeResultsView model =
  case validateModel model of
    Just validModel -> Calculator.ResultsView.view validModel
    Nothing -> div [] []
