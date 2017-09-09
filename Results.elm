module Results exposing (view, formatFloat)

import Html exposing (Html, text, table, thead, tbody, tr, th, td, div)
import Html.Attributes exposing (scope)
import Types exposing (ValidModel, Msg(..))

view : ValidModel -> Html Msg
view model =
  let
    bodyColHeaders = List.map colHeader (List.map toString model.fronts)
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

row : List Int -> Int -> Html Msg
row fronts rear =
  let
    rowHeader = th [scope "row"] [text (toString rear)]
    bodyCells = List.map (singleResultCell rear) fronts
  in
    tr [] (rowHeader :: bodyCells)

singleResultCell : Int -> Int -> Html Msg
singleResultCell rear front =
  let
    ratio = formatResult (gearRatio front rear)
  in
    td [] [text ratio]

gearRatio : Int -> Int -> Float
gearRatio front rear = (toFloat front / toFloat rear)

formatResult: Float -> String
formatResult result = formatFloat result 2

formatFloat : Float -> Int -> String
formatFloat f nDecimals =
  let
    shift = toFloat (10 ^ nDecimals)
    rounded = (toFloat (round (f * shift))) / shift
  in
    toString rounded
