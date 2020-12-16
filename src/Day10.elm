module Day10 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Bootstrap.Grid.Col exposing (xs)
import Dict exposing (..)
import Html exposing (a)
import Maybe
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Result
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


type alias SortedList a =
    List a


init : DayModel
init =
    DayModel
        { example = Just example2
        , updateDayModel = \model daymodel -> { model | day10 = daymodel }
        , input = NotAsked
        , inputFile = "day10_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    loadInput input
        |> differences
        |> onesProductThrees
        |> Just


part2 : String -> Maybe Int
part2 input =
    loadInput input
        |> differences
        |> splitOnThrees
        |> List.map considerDiffs
        |> List.product
        |> Just


splitOnThrees : List Int -> List (List Int)
splitOnThrees list =
    -- Turn into a string and back in order to use String.split
    List.foldr (\i s -> String.fromInt i ++ "\n" ++ s) "" list
        |> String.split "3\n"
        |> List.map (String.split "\n" >> List.filterMap String.toInt)


part3 : String -> Maybe Int
part3 input =
    loadInput input
        |> List.tail
        |> Maybe.withDefault []
        |> consider 0
        |> Just


partition : List (List Int) -> Dict (List Int) Int
partition results =
    List.foldl (\result state -> Dict.insert result ((Dict.get result state |> Maybe.withDefault 0) + 1) state) Dict.empty results


loadInput : String -> SortedList Int
loadInput input =
    String.split "\n" input
        |> List.filterMap String.toInt
        |> (\x -> x ++ [ 0, (Maybe.withDefault 0 <| List.maximum x) + 3 ])
        |> List.sort


differences : List Int -> List Int
differences list =
    let
        addOne : Int -> ( Int, List Int ) -> ( Int, List Int )
        addOne x state =
            case state of
                ( prev, ds ) ->
                    ( x, (x - prev) :: ds )
    in
    List.reverse <|
        case list of
            x :: xs ->
                List.foldl addOne ( x, [] ) xs |> Tuple.second

            _ ->
                []


ns : Int -> List Int -> Int
ns n l =
    List.filter ((==) n) l
        |> List.length
        |> Debug.log (String.fromInt n)


onesProductThrees : List Int -> Int
onesProductThrees deltas =
    ns 1 deltas * ns 3 deltas


consider : Int -> SortedList Int -> Int
consider prev remainder =
    let
        valid : Int -> Basics.Bool
        valid candidate =
            candidate - prev <= 3
    in
    case remainder of
        t :: [] ->
            if valid t then
                1

            else
                0

        x :: xs ->
            if valid x then
                consider x xs + consider prev xs

            else
                0

        _ ->
            0


considerDiffs : List Int -> Int
considerDiffs diffs =
    let
        considerHelp : List Int -> Int -> List Int -> Int
        considerHelp stem gap remainder =
            case remainder of
                x :: xs ->
                    if gap + x <= 3 then
                        considerHelp (stem ++ [ x ]) 0 xs + considerHelp stem (gap + x) xs

                    else
                        0

                _ ->
                    if gap == 0 then
                        1

                    else
                        0
    in
    considerHelp [] 0 diffs


example : String
example =
    """16
10
15
5
1
11
7
19
6
12
4"""


example2 : String
example2 =
    """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""
