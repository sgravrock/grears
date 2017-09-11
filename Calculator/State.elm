module Calculator.State exposing (model, update, validateModel)

import Array
import Calculator.Types exposing(Msg(..), Model, Diameter(..), ValidModel, ResultUnit(..), ValidResultUnit(..))


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
    GearInches -> Maybe.map ValidGearInches (toMaybeDiameter model.wheelDia)
    MphAt60Rpm -> Maybe.map ValidMphAt60Rpm (toMaybeDiameter model.wheelDia)

toMaybeInt : String -> Maybe Int
toMaybeInt s = Result.toMaybe (String.toInt s)

toMaybeDiameter : String -> Maybe Diameter
toMaybeDiameter d = Maybe.map DiameterInches (toMaybeFloat d)

toMaybeFloat : String -> Maybe Float
toMaybeFloat s = Result.toMaybe (String.toFloat s)



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
