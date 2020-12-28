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
    List Int


part1 : String -> Maybe Int
part1 =
    part 3


part2 : String -> Maybe Int
part2 =
    -- Should be 4 - but running very slow.
    part 2


part : Int -> String -> Maybe Int
part dimensions input =
    readInput dimensions input
        |> cycleN dimensions 6
        |> Set.size
        |> Just


example : String
example =
    """.#.
..#
###"""


addPoints : Coordinates -> Coordinates -> Coordinates
addPoints p1 p2 =
    List.map2 (+) p1 p2


getPointNeighbors : Int -> Coordinates -> Set Coordinates
getPointNeighbors dimensions point =
    Set.map (addPoints point) <| neighborDeltas dimensions


getAllNeighbors : Int -> Set Coordinates -> Set Coordinates
getAllNeighbors dimensions input =
    let
        considerPoint : Coordinates -> Set Coordinates -> Set Coordinates
        considerPoint point acc =
            Set.union acc <| getPointNeighbors dimensions point
    in
    Set.foldl considerPoint input input


neighborDeltasHelp : Int -> Set Coordinates
neighborDeltasHelp dimensions =
    let
        options =
            [ -1, 0, 1 ]

        addDimensionToPoint : Coordinates -> List Coordinates
        addDimensionToPoint point =
            List.map (\c -> c :: point) options

        addDimension : List Coordinates -> List Coordinates
        addDimension base =
            List.concatMap addDimensionToPoint base
    in
    List.foldl (\_ acc -> addDimension acc) [ [] ] (List.range 1 dimensions)
        |> Set.fromList
        |> Set.remove (List.repeat dimensions 0)


neighborDeltas2 : Set Coordinates
neighborDeltas2 =
    neighborDeltasHelp 2


neighborDeltas3 : Set Coordinates
neighborDeltas3 =
    neighborDeltasHelp 3


neighborDeltas4 : Set Coordinates
neighborDeltas4 =
    neighborDeltasHelp 4


neighborDeltas : Int -> Set Coordinates
neighborDeltas dimensions =
    case dimensions of
        2 ->
            neighborDeltas2

        3 ->
            neighborDeltas3

        4 ->
            neighborDeltas4

        n ->
            neighborDeltasHelp n


cycleN : Int -> Int -> Set Coordinates -> Set Coordinates
cycleN dimensions n input =
    List.foldl (\_ state -> cycle dimensions state) input <| List.range 1 n


cycle : Int -> Set Coordinates -> Set Coordinates
cycle dimensions input =
    let
        isActive : Coordinates -> Bool
        isActive point =
            let
                activeNeighbors =
                    getPointNeighbors dimensions point
                        |> Set.intersect input
                        |> Set.size
            in
            if Set.member point input then
                activeNeighbors == 2 || activeNeighbors == 3

            else
                activeNeighbors == 3
    in
    getAllNeighbors dimensions input
        |> Set.filter isActive



-- Input Parsing


readInput : Int -> String -> Set Coordinates
readInput dimensions input =
    let
        extra =
            List.repeat (dimensions - 2) 0

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
                            Just <| [ x, y ] ++ extra

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
