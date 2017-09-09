module Types exposing(Model, Msg(..))

import Array


type alias Model =
  { front : String
  , rear : Array.Array String
  }


type Msg = Front String | Rear Int String
