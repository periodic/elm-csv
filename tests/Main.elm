module Main exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tests exposing (..)


suite : Test
suite =
    Tests.all
