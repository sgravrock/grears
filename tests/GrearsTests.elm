module GrearsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Grears exposing (formatFloat)


-- suite : Test
-- suite =
--     todo "Implement our first test. See http://package.elm-lang.org/packages/elm-community/elm-test/latest for how to do this!"

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
        ]
