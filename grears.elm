module Grears exposing (..)

import Html exposing (Html, div, text, fieldset, legend, input)
import Html.Attributes exposing (type_, class, rel, href, size)
import Html.Events exposing (onInput)
import Array
import Types exposing (Model, Msg(..))
import Results


-- MODEL

model : Model
model =
  { fronts = List.repeat 3 ""
  , rears = List.repeat 11 ""
  }

validateModel : Model -> Maybe Model
validateModel model =
  let
    filtered = 
      { fronts = List.filter isValidInt model.fronts
      , rears = List.filter isValidInt model.rears
      }
  in
    if filtered.fronts == [] || filtered.rears == [] then
      Nothing
    else
      Just filtered



-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetFront i s ->
      { model | fronts = setAt i s model.fronts }
    SetRear i s ->
      { model | rears = setAt i s model.rears }

setAt : Int -> String -> List String -> List String
setAt i value list =
  let
    a = Array.set i value (Array.fromList list)
  in
    Array.toList a

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
      , div [] (List.indexedMap frontGearField model.fronts)
      ]
    , fieldset [] [
        legend [] [ text "Rear gears" ]
      , div [] (List.indexedMap rearGearField model.rears)
      ]
    , Results.view (validateModel model)
    ]

frontGearField : Int -> String -> Html Msg
frontGearField i value = intField (SetFront i) value

rearGearField : Int -> String -> Html Msg
rearGearField i value = intField (SetRear i) value

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
