module GrearsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Types exposing (Model, ValidModel, ResultUnit(..), ValidResultUnit(..))

import Calculator exposing (validateModel)
import Results exposing (formatFloat, calculateResult)


all : Test
all =
  describe "Grears"
    [ describe "formatFloat"
      [ test "with decimals" <|
        \() ->
          Expect.equal (formatFloat 1.234 2) "1.23"
      , test "without decimals" <|
        \() ->
          Expect.equal (formatFloat 1 2) "1"
      ]
    , describe "validateModel"
      [ test "filters invalid chainrings" <|
        \() ->
          let
            model = modelForGears ["", "bob", "1"] ["2"]
            expected = validModelForGears [1] [2]
          in
            Expect.equal (validateModel model) (Just expected)
      , test "filters invalid cogs" <|
        \() ->
          let
            model = modelForGears ["1"] ["", "three", "2"]
            expected = validModelForGears [1] [2]
          in
            Expect.equal (validateModel model) (Just expected)
      , test "requires at least one valid chainring" <|
        \() ->
          let
            model = modelForGears ["", "fifty two"] ["1"]
          in
            Expect.equal (validateModel model) Nothing
      , test "requires at least one valid cog" <|
        \() ->
          let
            model = modelForGears ["1"] ["", "fifty two"]
          in
            Expect.equal (validateModel model) Nothing
      , test "accepts Ratio" <|
        \() ->
          let
            model = modelForUnit Ratio
            expected = validModelForUnit ValidRatio
          in
            Expect.equal (validateModel model) (Just expected)
      , test "rejects gear inches without wheel size" <|
        \() ->
          let
            model = modelForUnit GearInches
          in
            Expect.equal (validateModel model) Nothing
      , test "accepts gear inches with wheel size" <|
        \() ->
          let
            tmp = modelForUnit GearInches
            model = { tmp | wheelDia = "1" }
            expected = validModelForUnit (ValidGearInches 1)
          in
            Expect.equal (validateModel model) (Just expected)
      ]
    , describe "calculateResult"
      [ test "calculates ratios" <|
        \() ->
          let
            result = calculateResult 52 11 ValidRatio
          in
            Expect.within (Expect.Relative 0.01) 4.73 result
      , test "calculates gear inches" <|
        \() ->
          let
            result = calculateResult 52 11 (ValidGearInches 27.32)
          in
            Expect.within (Expect.Relative 0.01) 129.1 result
      ]
    ]

modelForGears : List String -> List String -> Model
modelForGears fronts rears = 
  { fronts = fronts
  , rears = rears
  , unit = Ratio
  , wheelDia = ""
  }

validModelForGears : List Int -> List Int -> ValidModel
validModelForGears fronts rears = 
  { fronts = fronts
  , rears = rears
  , unit = ValidRatio
  }

modelForUnit : ResultUnit -> Model
modelForUnit unit =
  { fronts = ["1"]
  , rears = ["1"]
  , wheelDia = ""
  , unit = unit
  }

validModelForUnit : ValidResultUnit -> ValidModel
validModelForUnit unit =
  { fronts = [1]
  , rears = [1]
  , unit = unit
  }
