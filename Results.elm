module Results exposing (view)

import Html exposing (Html, div, text)
import Array
import Msg exposing (Msg)

view : String -> Array.Array String -> Html Msg
view front rears =
  div [] [ text "Ratios: ", text (formattedResults front rears) ]

formattedResults: String -> Array.Array String -> String
formattedResults front rears =
  let
    rs = List.map formatResult (results front rears)
  in
    String.join ", " rs

results : String -> Array.Array String -> List (Maybe Float)
results front rears = List.map (gearRatio front) (Array.toList rears)


gearRatio : String -> String -> Maybe Float
gearRatio front rear =
  case String.toInt front of
    Err _ -> Nothing
    Ok f -> case String.toInt rear of
      Err _ -> Nothing
      Ok r -> Just (toFloat f / toFloat r)


formatResult: Maybe Float -> String
formatResult result =
  case result of
    Just n -> formatFloat n 2
    Nothing -> ""

formatFloat : Float -> Int -> String
formatFloat f nDecimals =
  let
    shift = toFloat (10 ^ nDecimals)
    rounded = (toFloat (round (f * shift))) / shift
  in
    toString rounded
