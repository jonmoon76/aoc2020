module Day15Test exposing (..)

import Day15 exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


part2Test : Test
part2Test =
    describe "Part2"
        [ test "X" (\_ -> Expect.equal (Just 0) (part2 "14,1,17,0,3,20"))
        ]
