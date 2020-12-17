module Dayn exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
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
        , updateDayModel = \model daymodel -> { model | day8 = daymodel }
        , input = NotAsked
        , inputFile = "day8_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    Nothing


part2 : String -> Maybe Int
part2 input =
    Nothing


example : String
example =
    """
    """
