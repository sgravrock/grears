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
  { fronts = Array.repeat 3 ""
  , rears = Array.repeat 11 ""
  }



-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetFront i s ->
      { model | fronts = Array.set i s model.fronts }
    SetRear i s ->
      { model | rears = Array.set i s model.rears }


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
      , div [] (Array.toList (Array.indexedMap frontGearField model.fronts))
      ]
    , fieldset [] [
        legend [] [ text "Rear gears" ]
      , div [] (Array.toList (Array.indexedMap rearGearField model.rears))
      ]
    , Results.view model
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
