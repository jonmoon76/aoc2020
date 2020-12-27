module Day15 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
import Html exposing (th)
import Maybe
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Result
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)
import Utils exposing (..)


init : DayModel
init =
    DayModel
        { example = Just example
        , updateDayModel = \model daymodel -> { model | day15 = daymodel }
        , input = NotAsked
        , inputFile = "day15_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    Just (playGame 2020 input)


part2 : String -> Maybe Int
part2 input =
    --Just (playGame 30000000 input)
    Nothing


type alias State =
    { time : Int
    , previousGo : Int
    , previousRounds : Dict Int ( Int, Maybe Int )
    }


recurse : (a -> Result b a) -> a -> b
recurse f x =
    case f x of
        Ok a ->
            recurse f a

        Err b ->
            b


playGame : Int -> String -> Int
playGame maxTime input =
    let
        playGameHelp : State -> Result Int State
        playGameHelp state =
            (if state.time > 28200000 || modBy 100000 state.time == 0 then
                Debug.log (String.fromInt state.time) (Dict.size state.previousRounds)

             else
                state.time
            )
                |> always
                    (if state.time >= maxTime then
                        Err state.previousGo

                     else
                        Ok <| makeMove state
                    )
    in
    recurse playGameHelp <| seedMoves input


previousTime : Int -> State -> Maybe Int
previousTime n state =
    case Dict.get n state.previousRounds of
        Nothing ->
            Nothing

        Just ( a, _ ) ->
            Just a


updateState : Int -> State -> State
updateState n state =
    { state
        | previousRounds = Dict.insert n ( state.time, previousTime n state ) state.previousRounds
        , time = state.time + 1
        , previousGo = n
    }


makeMove : State -> State
makeMove state =
    case Dict.get state.previousGo state.previousRounds of
        Just ( a, Nothing ) ->
            updateState 0 state

        Just ( a, Just b ) ->
            updateState (a - b) state

        Nothing ->
            Debug.todo "Assert: Must have made previous move"


seedMoves : String -> State
seedMoves input =
    loadInput input |> List.foldl updateState (State 0 0 Dict.empty)


loadInput : String -> List Int
loadInput input =
    String.split "," input |> List.filterMap String.toInt


example : String
example =
    "0,3,6"
