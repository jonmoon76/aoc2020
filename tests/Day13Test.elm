module Day13Test exposing (..)

import Day13 exposing (..)
import Dict exposing (Dict(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html exposing (a)
import Set exposing (Set(..))
import Test exposing (..)
import Tuple exposing (..)



{-
   bezoutTests =
       describe "Bezout Identity Tests"
           [ test "5 23"
               (\_ -> Expect.equal ( -9, 2 ) (bezoutIdentity 5 23))
           , test "3 5"
               (\_ -> Expect.equal ( 2, -1 ) (bezoutIdentity 3 5))
           ]

-}
{-
   twoCongruencesTests =
       describe "Two Congruences Tests"
           [ test "(1,5), (2,3)"
               (\_ -> Expect.equal ( 5, 15 ) (solve2Congruences ( 0, 5 ) ( -1, 3 )))
           ]

-}


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


primes =
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 ]


testPrimes =
    List.indexedMap Tuple.pair primes


seedPrimes =
    -- Primes (Dict.fromList (List.indexedMap Tuple.pair primes)) 0
    emptyPrimes


nth n =
    case nthPrime seedPrimes n of
        ( p, _ ) ->
            p


factors n =
    case factorise seedPrimes n of
        ( [ a ], _ ) ->
            a

        _ ->
            1



{-
   factoriseTests =
       describe "factorise"
           [ test "3" (\_ -> Expect.equal 3 (factors 3))
           , test "5" (\_ -> Expect.equal 5 (factors 5))
           ]

-}


moduloProductTests =
    describe
        "moduloProduct"
        [ test "1 * 3 == 1 mod 2" (\_ -> Expect.equal 2 (moduloProduct 7 [ 2, 3, 5 ]))
        ]


nthPrimeTests =
    describe "nthPrime"
        (List.map
            (\( n, p ) -> test (Debug.toString ( n, p )) (\_ -> Expect.equal p (nth n)))
            testPrimes
        )


isPrimeTests =
    let
        checkPrime n =
            case isPrime seedPrimes n of
                ( result, _ ) ->
                    result
    in
    describe "isPrime"
        (List.map
            (\n -> test (Debug.toString n) (\_ -> Expect.true "Wrong" (checkPrime n == (Set.member n <| Set.fromList primes))))
            (List.range
                1
             <|
                (List.maximum primes
                    |> Maybe.withDefault 1
                )
            )
        )
