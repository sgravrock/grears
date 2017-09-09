import Html exposing (Html)
import Grears exposing (model, view, update)

main =
  Html.beginnerProgram { model = model, view = view, update = update }
