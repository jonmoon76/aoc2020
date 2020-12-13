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
        { example = Nothing
        , updateDayModel = \model daymodel -> { model | day6 = daymodel }
        , input = NotAsked
        , inputFile = "day6_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : Maybe String -> Maybe Int
part1 input =
    Nothing


part2 =
    part1
