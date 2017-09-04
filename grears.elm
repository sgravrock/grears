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

isValid : Model -> Bool
isValid model = isValidInt model.front && isValidInt model.rear

isValidInt : String -> Bool
isValidInt s =
  case String.toInt s of
    Ok _ -> True
    Err _ -> False

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
    , div [] [ text "Ratio: ", text (result model) ]
    ]

intField : (String -> Msg) -> String -> Html Msg
intField updateFunc value =
  input [ type_ "text", onInput updateFunc, class (fieldClass value) ] []

fieldClass : String -> String
fieldClass value =
  case value of
    "" -> ""
    _ -> if isValidInt value then "" else "invalid"

result : Model -> String
result model =
  case String.toInt model.front of
    Err _ -> ""
    Ok f -> case String.toInt model.rear of
      Err _ -> ""
      Ok r -> toString (toFloat f / toFloat r)
