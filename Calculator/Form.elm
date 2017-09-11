module Calculator.Form exposing (view)

import Html exposing (Html, div, text, fieldset, legend, input, select, option, label, span)
import Html.Attributes exposing (type_, class, size, id, for, selected)
import Html.Events exposing (onInput)
import Select

import Calculator.Types exposing (Msg(..), Model, ResultUnit(..))


view : (Msg -> a) -> Model -> Html a
view wrapMsg model =
  div []
    [ fieldset [] [
        legend [] [ text "Front gears" ]
      , div [] (List.indexedMap (frontGearField wrapMsg) model.fronts)
      ]
    , fieldset [] [
        legend [] [ text "Rear gears" ]
      , div [] (List.indexedMap (rearGearField wrapMsg) model.rears)
      ]
    , unitSelectBox wrapMsg model
    , if model.unit == GearInches then
        label []
          [ span [class "label"] [text "Wheel diameter in inches"]
          , wheelSizeField wrapMsg model.wheelDia
          ]
      else
        span [] []
    ]


frontGearField : (Msg -> a) -> Int -> String -> Html a
frontGearField wrapMsg i value = intField wrapMsg (SetFront i) value

rearGearField : (Msg -> a) -> Int -> String -> Html a
rearGearField wrapMsg i value = intField wrapMsg (SetRear i) value

intField : (Msg -> a) -> (String -> Msg) -> String -> Html a
intField wrapMsg updateFunc value =
  input [ type_ "text"
        , onInput (\s -> wrapMsg (updateFunc s))
        , class (fieldClass (value == ""  || isValidInt value))
        , size 2
        , Html.Attributes.value value
        ] []

wheelSizeField : (Msg -> a) -> String -> Html a
wheelSizeField wrapMsg value =
  input [ type_ "text"
        , onInput (\s -> wrapMsg (SetWheelDia s))
        , class (fieldClass (isValidFloat value))
        , size 5
        , Html.Attributes.value value
        ] []

fieldClass : Bool -> String
fieldClass isValid = if isValid then "" else "invalid"


unitSelectBox : (Msg -> a) -> Model -> Html a
unitSelectBox wrapMsg model =
  let
    units = [Ratio, GearInches]
  in
    label []
    [ span [class "label"] [text "Unit"]
    , Select.fromSelected_ units (\u -> wrapMsg (SetUnit u)) toString unitLabel model.unit
    ]

unitLabel : ResultUnit -> String
unitLabel u =
  case u of
    Ratio -> "Ratio"
    GearInches -> "Gear inches"


isValidInt : String -> Bool
isValidInt s =
  case String.toInt s of
    Ok _ -> True
    Err _ -> False

isValidFloat : String -> Bool
isValidFloat s =
  case String.toFloat s of
    Ok _ -> True
    Err _ -> False

