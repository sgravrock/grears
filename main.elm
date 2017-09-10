import Html exposing (Html, div)
import Html.Attributes exposing (rel, href)

import Calculator.Types exposing (Msg, Model)
import Calculator.State exposing (model, update)
import Calculator.View

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

view : Model -> Html Msg
view model =
  div []
    [
      Html.node "link" [ rel "stylesheet", href "grears.css" ] []
    , Calculator.View.view model
    ]
