import Html exposing (Html, div, text, fieldset, legend, input)
import Html.Attributes exposing (type_, class, rel, href, size)
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

type Msg = Front String | Rear Int String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Front s ->
      { model | front = s }
    Rear i s ->
      { model | rear = Array.set i s model.rear }

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
      , div [] (Array.toList (Array.indexedMap rearGearField model.rear))
      ]
    , div [] [ text "Ratios: ", text (formattedResults model) ]
    , div [] [ text "v: ", text (toString (String.length (stringAt 0 model.rear))) ]
    ]

rearGearField : Int -> String -> Html Msg
rearGearField i value = intField (Rear i) value

intField : (String -> Msg) -> String -> Html Msg
intField updateFunc value =
  input [ type_ "text"
        , onInput updateFunc
        , class (fieldClass value)
        , size 2
        ] []

fieldClass : String -> String
fieldClass value =
  case value of
    "" -> ""
    _ -> if isValidInt value then "" else "invalid"

results : Model -> List (Maybe Float)
results model = List.map (gearRatio model.front) (Array.toList model.rear)

gearRatio : String -> String -> Maybe Float
gearRatio front rear =
  case String.toInt front of
    Err _ -> Nothing
    Ok f -> case String.toInt rear of
      Err _ -> Nothing
      Ok r -> Just (toFloat f / toFloat r)

formattedResults : Model -> String
formattedResults model =
  let
    rs = List.map formatResult (results model)
  in
    String.join ", " rs

formatResult: Maybe Float -> String
formatResult result =
  case result of
    Just n -> toString n
    Nothing -> ""

