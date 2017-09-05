import Html exposing (Html, div, text, fieldset, legend, input)
import Html.Attributes exposing (type_, class, rel, href)
import Html.Events exposing (onInput)
import Array


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL


type alias Model =
  { front : String
  , rear : Array.Array String
  }

model : Model
model =
  { front = ""
  , rear = Array.fromList (List.repeat 11 "")
  }


-- UPDATE

type Msg = Front String | Rear String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Front s ->
      { model | front = s }
    Rear s ->
      { model | rear = Array.set 0 s model.rear }

isValid : Model -> Bool
isValid model = isValidInt model.front && isValidInt (stringAt 0 model.rear)

isValidInt : String -> Bool
isValidInt s =
  case String.toInt s of
    Ok _ -> True
    Err _ -> False

stringAt : Int -> Array.Array String -> String
stringAt i a =
  case Array.get i a of
    Just s -> s
    Nothing -> ""


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
      , intField Rear (stringAt 0 model.rear)
      ]
    , div [] [ text "Ratio: ", text (result model) ]
    , div [] [ text "v: ", text (toString (String.length (stringAt 0 model.rear))) ]
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
    Ok f -> case String.toInt (stringAt 0 model.rear) of
      Err _ -> ""
      Ok r -> toString (toFloat f / toFloat r)
