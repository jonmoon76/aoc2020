module Day14Test exposing (..)

import Day14 exposing (..)
import Dict exposing (Dict(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (a)
import Set exposing (Set(..))
import Test exposing (..)
import Tuple exposing (..)


decomposeTests : Test
decomposeTests =
    describe "Decompose/Recompose"
        (List.map
            (\x -> test (String.fromInt x) (\_ -> Expect.equal x (recompose <| decompose x)))
            [ 44160365643 ]
        )



{-
   bezoutTests : Test
   bezoutTests =
       describe "Bezout Identity Tests"
           [ test "5 23"
               (\_ -> Expect.equal ( -9, 2 ) (bezoutIdentity 5 23))
           , test "3 5"
               (\_ -> Expect.equal ( 2, -1 ) (bezoutIdentity 3 5))
           ]

-}


part1Test : Test
part1Test =
    describe "Part 1 Test"
        [ test "X" (\_ -> Expect.equal (Just 44160366155) (part1 testProgram))
        ]


part2Test : Test
part2Test =
    describe "Part 2 Test"
        [ test "X" (\_ -> Expect.equal (Just 0) (part2 testProgram))
        ]


expandMaskTest : Test
expandMaskTest =
    describe "Expand Mask"
        [ test "1" (\_ -> Expect.equal 1 (List.length <| expandMask [ Zero ]))
        , test "2" (\_ -> Expect.equal 1 (List.length <| expandMask [ One ]))
        , test "3" (\_ -> Expect.equal 2 (List.length <| expandMask [ Unspecified ]))
        , test "4" (\_ -> Expect.equal 4 (List.length <| expandMask [ Unspecified, Unspecified ]))
        , test "8" (\_ -> Expect.equal 8 (List.length <| expandMask [ Unspecified, Unspecified, Unspecified ]))
        , test "N" (\_ -> Expect.equal 2 (List.length <| expandMask <| Unspecified :: List.repeat 35 Zero))
        , test "N2" (\_ -> Expect.equal 2 (List.length <| expandMask <| List.reverse <| Unspecified :: List.repeat 35 Zero))
        , test "N3" (\_ -> Expect.equal 512 (List.length <| expandMask <| List.concat <| List.repeat 9 <| Unspecified :: List.repeat 3 Zero))
        ]


testMask : List MaskField
testMask =
    [ Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, Unspecified, One, Unspecified, Unspecified, Unspecified, Unspecified, Zero, Unspecified ]


testProgram : String
testProgram =
    """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""
