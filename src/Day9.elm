module Day9 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
import Html exposing (a)
import Maybe
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Result
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


init : DayModel
init =
    DayModel
        { example = Just example
        , updateDayModel = \model daymodel -> { model | day9 = daymodel }
        , input = NotAsked
        , inputFile = "day9_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    loadInput input
        |> findBad 25 []


part2 : String -> Maybe Int
part2 input =
    contiguousSum (part1 input |> Maybe.withDefault 0) (loadInput input)


loadInput : String -> List Int
loadInput input =
    String.split "\n" input
        |> List.filterMap String.toInt


example : String
example =
    """
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"""


tailDrop : Int -> List a -> List a
tailDrop n l =
    List.reverse l |> List.drop n |> List.reverse


findBad : Int -> List Int -> List Int -> Maybe Int
findBad windowSize preamble remaining =
    let
        helper : Int -> List Int -> Maybe Int
        helper x xs =
            if List.length preamble < windowSize then
                findBad windowSize (x :: preamble) xs

            else if hasPairWhichSums preamble x then
                findBad windowSize (x :: tailDrop 1 preamble) xs

            else
                Just x
    in
    case remaining of
        x :: xs ->
            helper x xs

        _ ->
            Nothing


hasPairWhichSums : List Int -> Int -> Bool
hasPairWhichSums input target =
    let
        helper : Int -> List Int -> Bool
        helper x xs =
            if Set.member (target - x) (Set.fromList xs) then
                True

            else
                hasPairWhichSums xs target
    in
    case input of
        x :: xs ->
            helper x xs

        _ ->
            False


contiguousSum : Int -> List Int -> Maybe Int
contiguousSum target input =
    case input of
        x :: xs ->
            case contiguousSumAnchored target input [] of
                Just n ->
                    Just n

                _ ->
                    contiguousSum target xs

        _ ->
            Nothing


contiguousSumAnchored : Int -> List Int -> List Int -> Maybe Int
contiguousSumAnchored target input current =
    if List.sum current == target then
        Maybe.map2 (+) (List.minimum current) (List.maximum current)

    else if List.sum current > target then
        Nothing

    else
        case input of
            x :: xs ->
                contiguousSumAnchored target xs (x :: current)

            _ ->
                Nothing
