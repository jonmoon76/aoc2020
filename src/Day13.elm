module Day13 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
import Html exposing (a, b, th)
import Maybe
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Result
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


type alias Congruence =
    ( Int, Int )


init : DayModel
init =
    DayModel
        { example = Just example
        , updateDayModel = \model daymodel -> { model | day13 = daymodel }
        , input = NotAsked
        , inputFile = "day13_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    let
        startTime =
            loadPart1Seed input

        busses =
            List.map Tuple.second (loadCongruences input)
    in
    case search startTime busses of
        ( bus, time ) ->
            Just (bus * (time - startTime))


search : Int -> List Int -> ( Int, Int )
search n busses =
    let
        findBus : List Int -> Maybe Int
        findBus remainingBusses =
            case remainingBusses of
                [] ->
                    Nothing

                bus :: xs ->
                    if modBy bus n == 0 then
                        Just bus

                    else
                        findBus xs
    in
    case findBus busses of
        Just bus ->
            ( bus, n )

        Nothing ->
            search (n + 1) busses


part2 : String -> Maybe Int
part2 input =
    solveNCongruences (loadCongruences input) |> Just


type alias EuclidTerm =
    { r : Int, s : Int, t : Int }


extendedEuclid : EuclidTerm -> EuclidTerm -> EuclidTerm
extendedEuclid prev cur =
    let
        q =
            prev.r // cur.r

        r_next =
            modBy cur.r prev.r

        s_next =
            prev.s - q * cur.s

        t_next =
            prev.t - q * cur.t
    in
    if r_next == 0 then
        cur

    else
        extendedEuclid cur (EuclidTerm r_next s_next t_next)


bezoutIdentity : Int -> Int -> ( Int, Int )
bezoutIdentity n1 n2 =
    let
        { r, s, t } =
            extendedEuclid { r = n1, s = 1, t = 0 } { r = n2, s = 0, t = 1 }
    in
    ( s, t )


moduloProduct : Int -> List Int -> Int
moduloProduct n factors =
    trace "moduloProduct"
        ( n, factors )
        (\_ ->
            List.concatMap (modBy n >> factorise emptyPrimes >> Tuple.first) factors
                |> List.foldl (\f p -> modBy n (f * p)) 1
        )


solve2Congruences : Congruence -> Congruence -> Congruence
solve2Congruences c1 c2 =
    let
        ( a1, n1 ) =
            c1

        ( a2, n2 ) =
            c2

        ( m1, m2 ) =
            bezoutIdentity n1 n2

        multiply x y =
            x * y

        tripleProduct : Int -> Int -> Int -> Int
        tripleProduct a m n =
            --Debug.todo ("moduloProduct" ++ Debug.toString ( n3, [ a, m, n ] ))
            moduloProduct n3 [ a, m, n ]

        --multiply a m |> modBy n3 |> multiply n |> modBy n3
        a3_ =
            a1 * m2 * n2 + a2 * m1 * n1

        a3__ =
            tripleProduct a1 m2 n2 + tripleProduct a2 m1 n1

        a3 =
            modBy n3 a3__

        n3 =
            n1 * n2
    in
    ( a3, n3 )


solveNCongruences : List Congruence -> Int
solveNCongruences list =
    case list of
        [ ( a, n ) ] ->
            a

        [] ->
            0

        c1 :: c2 :: cs ->
            solveNCongruences (solve2Congruences c1 c2 :: cs)


loadInput : String -> ( String, String )
loadInput input =
    case String.split "\n" input of
        [ line1, line2 ] ->
            ( line1, line2 )

        _ ->
            ( "", "" )


loadPart1Seed : String -> Int
loadPart1Seed input =
    loadInput input
        |> Tuple.first
        |> String.toInt
        |> Maybe.withDefault 0


loadCongruences : String -> List Congruence
loadCongruences input =
    loadInput input
        |> Tuple.second
        |> String.split ","
        |> List.indexedMap (\i a -> ( i, String.toInt a ))
        |> List.filterMap
            (\c ->
                case c of
                    ( a, Just n ) ->
                        Just ( negate a, n )

                    _ ->
                        Nothing
            )


example : String
example =
    """939
7,13,x,x,59,x,31,19"""


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
            case factoriseHelp False primes candidate of
                ( [ p ], newPrimes ) ->
                    True

                _ ->
                    False

        findNext_ : Primes -> Int -> ( Int, Primes )
        findNext_ primes candidate =
            if isPrimeHelp primes candidate then
                ( candidate, Dict.insert (Dict.size primes) candidate primes )

            else
                findNext_ primes (candidate + 1)

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
    trace "nthPrime"
        ( n, oldPrimes )
        (\_ ->
            List.foldl
                (\_ ( p, primes ) -> findNext primes)
                ( Dict.get n oldPrimes |> Maybe.withDefault 0, oldPrimes )
                (List.range (Dict.size oldPrimes) n)
        )
