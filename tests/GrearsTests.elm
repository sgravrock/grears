module GrearsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Grears exposing (validateModel, filterMapResult)
import Results exposing (formatFloat)


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
            model =
              { fronts = ["", "bob", "1"]
              , rears = ["2"]
              }
            expected =
              { fronts = [1]
              , rears = [2]
              }
          in
            Expect.equal (validateModel model) (Just expected)
      , test "filters invalid cogs" <|
        \() ->
          let
            model =
              { fronts = ["1"]
              , rears = ["", "three", "2"]
              }
            expected =
              { fronts = [1]
              , rears = [2]
              }
          in
            Expect.equal (validateModel model) (Just expected)
      , test "requires at least one valid chainring" <|
        \() ->
          let
            model =
              { fronts = ["", "fifty two"]
              , rears = ["1"]
              }
          in
            Expect.equal (validateModel model) Nothing
      , test "requires at least one valid cog" <|
        \() ->
          let
            model =
              { fronts = ["1"]
              , rears = ["", "fifty two"]
              }
          in
            Expect.equal (validateModel model) Nothing
      ]
    , describe "filterMapResult"
      [ test "like List.filterMap but for Result instead of Maybe" <|
        \() ->
          let
            input = [(Ok "foo"), (Err "nope")]
          in
            Expect.equal (filterMapResult input) ["foo"]
      ]
    ]
