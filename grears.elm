import Html exposing (Html, div, text, fieldset, legend, input)
import Html.Attributes exposing (type_, class, rel, href)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL


type alias Model =
  { front : String
  , rear : String
  }

model : Model
model =
  { front = ""
  , rear = ""
  }


-- UPDATE

type Msg = Front String | Rear String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Front s ->
      { model | front = s }
    Rear s ->
      { model | rear = s }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ 
      Html.node "link" [ rel "stylesheet", href "grears.css" ] []
    , fieldset [] [
        legend [] [ text "Front gears" ]
      , intField Front model.front
      ]
    , fieldset [] [
        legend [] [ text "Rear gears" ]
      , intField Rear model.rear
      ]
    , div [] [ text "Front: ", text (display model.front) ]
    , div [] [ text "Rear: ", text (display model.rear) ]
    ]

intField : (String -> Msg) -> String -> Html Msg
intField updateFunc value =
  input [ type_ "text", onInput updateFunc, class (fieldClass value) ] []

fieldClass : String -> String
fieldClass value =
  case value of
    "" -> ""
    _ -> case String.toInt value of
      Ok _ -> ""
      Err _ -> "invalid"

display : String -> String
display value = 
  case String.toInt value of
    Ok _ -> value
    Err _ -> ""
