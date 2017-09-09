module Types exposing(Model, Msg(..))

type alias Model =
  { fronts : List String
  , rears : List String
  }


type Msg = SetFront Int String | SetRear Int String
