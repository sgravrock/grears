module Grears exposing (..)

import Html exposing (Html, div, text, fieldset, legend, input, select, option, label, span)
import Html.Attributes exposing (type_, class, rel, href, size, id, for, selected)
import Html.Events exposing (onInput, on)
import Array
import Select


import Types exposing (Model, ValidModel, ResultUnit(..), ValidResultUnit(..), Msg(..))
import Results


-- MODEL

model : Model
model =
  { fronts = List.repeat 3 ""
  , rears = List.repeat 11 ""
  , unit = Ratio
  , wheelDia = ""
  }

validateModel : Model -> Maybe ValidModel
validateModel model =
  let
    fronts = onlyInts model.fronts
    rears = onlyInts model.rears
  in
    if fronts == [] || rears == [] then
      Nothing
    else
      case validateUnits model of
        Nothing -> Nothing
        Just unit -> Just
          { fronts = fronts
          , rears = rears
          , unit = unit
          }

onlyInts : List String -> List Int
onlyInts strings = List.filterMap identity (List.map toMaybeInt strings)

validateUnits : Model -> Maybe ValidResultUnit
validateUnits model =
  case model.unit of
    Ratio -> Just ValidRatio
    GearInches -> Maybe.map ValidGearInches (toMaybeFloat model.wheelDia)

toMaybeInt : String -> Maybe Int
toMaybeInt s = Result.toMaybe (String.toInt s)

toMaybeFloat : String -> Maybe Float
toMaybeFloat s = Result.toMaybe (String.toFloat s)


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    SetFront i s ->
      { model | fronts = setAt i s model.fronts }
    SetRear i s ->
      { model | rears = setAt i s model.rears }
    SetUnit u -> { model | unit = u }
    SetWheelDia s -> { model | wheelDia = s }
    

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
    , unitSelectBox model
    , label []
      [ span [class "label"] [text "Wheel diameter in inches"]
      , intField SetWheelDia model.wheelDia
      ]
    , maybeResultsView model
    ]

unitSelectBox : Model -> Html Msg
unitSelectBox model =
  let
    units = [Ratio, GearInches]
  in
    label []
    [ span [class "label"] [text "Unit"]
    , Select.fromSelected_ units SetUnit toString unitLabel model.unit
    ]

unitLabel : ResultUnit -> String
unitLabel u =
  case u of
    Ratio -> "Ratio"
    GearInches -> "Gear inches"
    
maybeResultsView : Model -> Html Msg
maybeResultsView model =
  case validateModel model of
    Just validModel -> Results.view validModel
    Nothing -> div [] []

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
