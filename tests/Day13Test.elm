module Day13Test exposing (..)

import Day13 exposing (..)
import Dict exposing (Dict(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (a)
import Set exposing (Set(..))
import Test exposing (..)
import Tuple exposing (..)


bezoutTests : Test
bezoutTests =
    describe "Bezout Identity Tests"
        [ test "5 23"
            (\_ -> Expect.equal ( -9, 2 ) (bezoutIdentity 5 23))
        , test "3 5"
            (\_ -> Expect.equal ( 2, -1 ) (bezoutIdentity 3 5))
        ]


twoCongruencesTests : Test
twoCongruencesTests =
    describe "Two Congruences Tests"
        [ test "(1,5), (2,3)"
            (\_ -> Expect.equal ( 5, 15 ) (solve2Congruences ( 0, 5 ) ( -1, 3 )))
        ]


part2Tests : Test
part2Tests =
    let
        cases =
            [ ( "\n5,3", 5 )
            , ( "\n17,x,13,19", 3417 )
            , ( "\n67,7,59,61", 754018 )
            , ( "\n67,x,7,59,61", 779210 )
            , ( "\n67,7,x,59,61", 1261476 )
            , ( "\n1789,37,47,1889", 1202161486 )
            ]
    in
    describe "Part Two"
        (List.map
            (\( x, y ) -> test x (\_ -> Expect.equal (Just y) (part2 x)))
            [ ( "\n5,3", 5 )
            ]
        )


isCorrect : List Congruence -> Bool
isCorrect cs =
    let
        soln =
            solveNCongruences cs

        a c =
            Tuple.first c

        n c =
            Tuple.second c
    in
    List.foldl
        (\c r ->
            if modBy (n c) soln /= modBy (n c) (a c) then
                Debug.log "Fail"
                    (Debug.toString ( c, soln ))
                    |> always False

            else
                r
        )
        True
        cs


part2Tests2 : Test
part2Tests2 =
    let
        input =
            "\n23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,479,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,373,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19"

        congruences =
            loadCongruences input

        cases =
            List.range 1 (List.length congruences)
                |> List.map (\x -> List.take x congruences)
    in
    describe "Part Two 2"
        (List.map
            (\x -> test (Debug.toString x) (\_ -> Expect.true "Invalid" (isCorrect x)))
            cases
        )
