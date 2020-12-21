module PrimesTest exposing (..)

import Dict exposing (Dict(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Primes exposing (..)
import Set exposing (Set(..))
import Test exposing (..)
import Tuple exposing (..)


primes : List Int
primes =
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 ]


testPrimes : List ( Int, Int )
testPrimes =
    List.indexedMap Tuple.pair primes


seedPrimes : Primes
seedPrimes =
    -- Primes (Dict.fromList (List.indexedMap Tuple.pair primes)) 0
    emptyPrimes


factoriseTests : Test
factoriseTests =
    let
        factors n =
            case factorise seedPrimes n of
                ( [ a ], _ ) ->
                    a

                _ ->
                    1
    in
    describe "factorise"
        [ test "3" (\_ -> Expect.equal 3 (factors 3))
        , test "5" (\_ -> Expect.equal 5 (factors 5))
        ]


moduloProductTests : Test
moduloProductTests =
    describe
        "moduloProduct"
        [ test "1 * 3 == 1 mod 2" (\_ -> Expect.equal 2 (moduloProduct 7 [ 2, 3, 5 ]))
        ]


nthPrimeTests : Test
nthPrimeTests =
    let
        nth n =
            case nthPrime seedPrimes n of
                ( p, _ ) ->
                    p
    in
    describe "nthPrime"
        (List.map
            (\( n, p ) -> test (Debug.toString ( n, p )) (\_ -> Expect.equal p (nth n)))
            testPrimes
        )


isPrimeTests : Test
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
