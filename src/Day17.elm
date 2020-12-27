module Day17 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
import List.Extra as List
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
        , updateDayModel = \model daymodel -> { model | day17 = daymodel }
        , input = NotAsked
        , inputFile = "day17_input.txt"
        , part1 = part1
        , part2 = part2
        }


type alias Coordinates =
    ( Int, Int, Int )


part1 : String -> Maybe Int
part1 input =
    readInput input
        |> cycleN 6
        |> Set.size
        |> Just


part2 : String -> Maybe Int
part2 input =
    Nothing


example : String
example =
    """.#.
..#
###"""


addPoints : Coordinates -> Coordinates -> Coordinates
addPoints ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


getPointNeighbors : Coordinates -> Set Coordinates
getPointNeighbors point =
    Set.map (addPoints point) neighborDeltas


getAllNeighbors : Set Coordinates -> Set Coordinates
getAllNeighbors input =
    let
        considerPoint : Coordinates -> Set Coordinates -> Set Coordinates
        considerPoint point acc =
            Set.union acc <| getPointNeighbors point
    in
    Set.foldl considerPoint input input


neighborDeltas : Set Coordinates
neighborDeltas =
    let
        options =
            [ -1, 0, 1 ]
    in
    options
        |> List.andThen
            (\x ->
                options
                    |> List.andThen
                        (\y ->
                            options
                                |> List.andThen
                                    (\z -> [ ( x, y, z ) ])
                        )
            )
        |> Set.fromList
        |> Set.remove ( 0, 0, 0 )


cycleN : Int -> Set Coordinates -> Set Coordinates
cycleN n input =
    List.foldl (\_ state -> cycle state) input <| List.range 1 n


cycle : Set Coordinates -> Set Coordinates
cycle input =
    let
        isActive : Coordinates -> Bool
        isActive point =
            let
                activeNeighbors =
                    getPointNeighbors point
                        |> Set.intersect input
                        |> Set.size
            in
            if Set.member point input then
                activeNeighbors == 2 || activeNeighbors == 3

            else
                activeNeighbors == 3
    in
    getAllNeighbors input
        |> Set.filter isActive



-- Input Parsing


readInput : String -> Set Coordinates
readInput input =
    let
        z =
            0

        indices : List a -> List Int
        indices l =
            List.range 0 (List.length l - 1)

        readRow : Int -> String -> List Coordinates
        readRow y row =
            let
                chars =
                    String.toList row
            in
            chars
                |> List.map2
                    (\x c ->
                        if c == '#' then
                            Just ( x, y, z )

                        else
                            Nothing
                    )
                    (indices chars)
                |> List.filterMap identity

        readRows : List String -> List Coordinates
        readRows rows =
            List.map2 readRow (indices rows) rows
                |> List.concat
    in
    String.split "\n" input
        |> readRows
        |> Set.fromList
