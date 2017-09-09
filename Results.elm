module Results exposing (view, formatFloat)

import Html exposing (Html, text, table, thead, tbody, tr, th, td)
import Html.Attributes exposing (scope)
import Array
import Types exposing (Model, Msg(..))

view : Model -> Html Msg
view model =
  table []
    [ thead []
        [ tr []
          [ th [] []
          , th [scope "col"] [text model.front]
          ]
        ]
    , tbody [] (List.map (row model.front) (Array.toList model.rear))
    ]

row : String -> String -> Html Msg
row front rear =
  let
    ratio = formatResult (gearRatio front rear)
  in
    tr []
      [ th [scope "row"] [text rear]
      , td [] [text ratio]
      ]


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
