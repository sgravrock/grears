import Html exposing (Html, div)
import Html.Attributes exposing (rel, href)

import Calculator
import Types exposing (Model, Msg)

main =
  Html.beginnerProgram
    { model = Calculator.model
    , view = view
    , update = Calculator.update
    }

view : Model -> Html Msg
view model =
  div []
    [
      Html.node "link" [ rel "stylesheet", href "grears.css" ] []
    , Calculator.view model
    ]
