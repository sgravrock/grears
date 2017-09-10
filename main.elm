import Html exposing (Html, div)
import Html.Attributes exposing (rel, href)

import Grears
import Types exposing (Model, Msg)

main =
  Html.beginnerProgram
    { model = Grears.model
    , view = view
    , update = Grears.update
    }

view : Model -> Html Msg
view model =
  div []
    [
      Html.node "link" [ rel "stylesheet", href "grears.css" ] []
    , Grears.view model
    ]
