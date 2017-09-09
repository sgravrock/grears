module Types exposing(Model, ValidModel, ResultUnit(..), ValidResultUnit(..), Msg(..))

type ResultUnit
  = Ratio
  | GearInches

type alias Model =
  { fronts : List String
  , rears : List String
  , unit : ResultUnit
  , wheelDia: String
  }

type ValidResultUnit
  = ValidRatio
  | ValidGearInches Float -- wheel diameter in inches

type alias ValidModel =
  { fronts : List Int
  , rears : List Int
  , unit : ValidResultUnit
  }


type Msg
  = SetFront Int String
  | SetRear Int String
  | SetUnit ResultUnit
  | SetWheelDia String
