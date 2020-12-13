module Day6 exposing (..)

import Array exposing (Array(..))
import Dict exposing (..)
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
        , updateDayModel = \model daymodel -> { model | day6 = daymodel }
        , input = NotAsked
        , inputFile = "day6_input.txt"
        , part1 = part1
        , part2 = part2
        }


example : String
example =
    """abc

a
b
c

ab
ac

a
a
a
a

b"""


part1 : String -> Maybe Int
part1 input =
    readGroups input
        |> List.map (List.foldl Set.union Set.empty)
        |> List.map Set.size
        |> List.sum
        |> Just


part2 : String -> Maybe Int
part2 input =
    let
        all : Set Char
        all =
            Set.fromList <| String.toList "abcdefghijklmnopqrstuvwxyz"
    in
    readGroups input
        |> List.map (List.foldl Set.intersect all)
        |> List.map Set.size
        |> List.sum
        |> Just


readGroups : String -> List (List (Set Char))
readGroups input =
    String.split "\n\n" input
        |> List.map readGroup


readGroup : String -> List (Set Char)
readGroup input =
    String.split "\n" input
        |> List.map String.toList
        |> List.map Set.fromList
