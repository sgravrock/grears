module Types exposing(Model, Msg(..))

import Array


type alias Model =
  { fronts : Array.Array String
  , rears : Array.Array String
  }


type Msg = SetFront Int String | SetRear Int String
