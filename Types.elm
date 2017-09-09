module Types exposing(Model, ValidModel, Msg(..))

type alias Model =
  { fronts : List String
  , rears : List String
  }

type alias ValidModel =
  { fronts: List Int
  , rears: List Int
  }


type Msg = SetFront Int String | SetRear Int String
