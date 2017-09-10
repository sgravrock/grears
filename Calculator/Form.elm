module Calculator.Form exposing (view)

import Html exposing (Html, div, text, fieldset, legend, input, select, option, label, span)
import Html.Attributes exposing (type_, class, size, id, for, selected)
import Html.Events exposing (onInput)
import Select

import Calculator.Types exposing (Msg(..), Model, ResultUnit(..))


view : Model -> Html Msg
view model =
  div []
    [ fieldset [] [
        legend [] [ text "Front gears" ]
      , div [] (List.indexedMap frontGearField model.fronts)
      ]
    , fieldset [] [
        legend [] [ text "Rear gears" ]
      , div [] (List.indexedMap rearGearField model.rears)
      ]
    , unitSelectBox model
    , if model.unit == GearInches then
        label []
          [ span [class "label"] [text "Wheel diameter in inches"]
          , wheelSizeField model.wheelDia
          ]
      else
        span [] []
    ]


frontGearField : Int -> String -> Html Msg
frontGearField i value = intField (SetFront i) value

rearGearField : Int -> String -> Html Msg
rearGearField i value = intField (SetRear i) value

intField : (String -> Msg) -> String -> Html Msg
intField updateFunc value =
  input [ type_ "text"
        , onInput updateFunc
        , class (fieldClass (value == ""  || isValidInt value))
        , size 2
        , Html.Attributes.value value
        ] []

wheelSizeField : String -> Html Msg
wheelSizeField value =
  input [ type_ "text"
        , onInput SetWheelDia
        , class (fieldClass (isValidFloat value))
        , size 5
        , Html.Attributes.value value
        ] []

fieldClass : Bool -> String
fieldClass isValid = if isValid then "" else "invalid"


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

