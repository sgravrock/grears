module Calculator.Types exposing (Msg(..), ResultUnit(..), Model, Diameter(..), ValidModel, ValidResultUnit(..))

type Msg
  = SetFront Int String
  | SetRear Int String
  | SetUnit ResultUnit
  | SetWheelDia String

type ResultUnit
  = Ratio
  | GearInches

type alias Model =
  { fronts : List String
  , rears : List String
  , unit : ResultUnit
  , wheelDia: String
  }

type Diameter = DiameterInches Float

type ValidResultUnit
  = ValidRatio
  | ValidGearInches Diameter

type alias ValidModel =
  { fronts : List Int
  , rears : List Int
  , unit : ValidResultUnit
  }
