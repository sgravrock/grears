module Results exposing (view, formatFloat)

import Html exposing (Html, text, table, thead, tbody, tr, th, td, div)
import Html.Attributes exposing (scope)
import Types exposing (Model, Msg(..))

view : Maybe Model -> Html Msg
view maybeModel =
  case maybeModel of
    Just model -> validView model
    Nothing -> div [] []

validView : Model -> Html Msg
validView model =
  let
    bodyColHeaders = List.map colHeader model.fronts
    colHeaders = emptyHeader :: bodyColHeaders
  in
    table []
      [ thead [] [tr [] colHeaders]
      , tbody [] (List.map (row model.fronts) model.rears)
      ]


emptyHeader : Html Msg
emptyHeader = th [] []

colHeader : String -> Html Msg
colHeader label =  th [scope "col"] [text label]

row : List String -> String -> Html Msg
row fronts rear =
  let
    rowHeader = th [scope "row"] [text rear]
    bodyCells = List.map (singleResultCell rear) fronts
  in
    tr [] (rowHeader :: bodyCells)

singleResultCell : String -> String -> Html Msg
singleResultCell rear front =
  let
    ratio = formatResult (gearRatio front rear)
  in
    td [] [text ratio]

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
