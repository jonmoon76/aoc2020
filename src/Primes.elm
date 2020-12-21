module Primes exposing (..)

import Dict exposing (..)
import Maybe


type alias Primes =
    Dict Int Int


factoriseHelp : Bool -> Primes -> Int -> ( List Int, Primes )
factoriseHelp expand oldPrimes n =
    let
        checkFactor primes m factors i =
            if m == 1 then
                ( factors, primes )

            else if not expand && i == Dict.size primes then
                ( m :: factors, primes )

            else
                case nthPrime primes i of
                    ( p, newPrimes ) ->
                        if modBy p m == 0 then
                            checkFactor newPrimes (m // p) (p :: factors) i

                        else
                            checkFactor newPrimes m factors (i + 1)
    in
    if n == 0 then
        ( [ 0 ], oldPrimes )

    else
        checkFactor oldPrimes n [] 0


trace : String -> a -> (() -> x) -> x
trace label args fx =
    let
        label_ =
            label ++ " " ++ Debug.toString args

        _ =
            Debug.log label_ "ENTER"
    in
    let
        x =
            fx ()

        _ =
            Debug.log label_ x
    in
    x


factorise : Primes -> Int -> ( List Int, Primes )
factorise primes n =
    trace "factorise"
        n
        (\_ ->
            factoriseHelp True primes n
        )


emptyPrimes : Primes
emptyPrimes =
    Dict.empty


isPrime : Primes -> Int -> ( Bool, Primes )
isPrime primes n =
    case factorise primes n of
        ( [ p ], newPrimes ) ->
            ( True, newPrimes )

        ( _, newPrimes ) ->
            ( False, newPrimes )


nthPrime : Primes -> Int -> ( Int, Primes )
nthPrime oldPrimes n =
    let
        isPrimeHelp primes candidate =
            trace "isPrimesHelp"
                candidate
                (\_ ->
                    case factoriseHelp False primes candidate of
                        ( [ p ], newPrimes ) ->
                            True

                        _ ->
                            False
                )

        findNext_ : Primes -> Int -> ( Int, Primes )
        findNext_ primes candidate =
            if isPrimeHelp primes candidate then
                ( candidate, Dict.insert (Dict.size primes) candidate primes )

            else
                findNext_ primes (candidate + 2)

        findNext : Primes -> ( Int, Primes )
        findNext primes =
            case List.maximum <| Dict.values primes of
                Nothing ->
                    ( 2, Dict.insert 0 2 primes )

                Just 2 ->
                    ( 3, Dict.insert 1 3 primes )

                Just p ->
                    findNext_ primes (p + 2)
    in
    List.foldl
        (\_ ( p, primes ) -> findNext primes)
        ( Dict.get n oldPrimes |> Maybe.withDefault 0, oldPrimes )
        (List.range (Dict.size oldPrimes) n)


moduloProduct : Int -> List Int -> Int
moduloProduct n factors =
    trace "moduloProduct"
        ( n, factors )
        (\_ ->
            List.concatMap (modBy n >> factorise emptyPrimes >> Tuple.first) factors
                |> List.foldl (\f p -> modBy n (f * p)) 1
        )
