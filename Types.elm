module Types exposing(Model, Msg(..))

import Array


type alias Model =
  { front : String
  , rears : Array.Array String
  }


type Msg = SetFront String | SetRear Int String
