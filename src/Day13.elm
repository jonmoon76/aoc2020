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


solve2Congruences : Congruence -> Congruence -> Congruence
solve2Congruences c1 c2 =
    let
        ( a1, n1 ) =
            c1

        ( a2, n2 ) =
            c2

        ( m1, m2 ) =
            bezoutIdentity n1 n2

        a3 =
            a1 * m2 * n2 + a2 * m1 * n1

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
