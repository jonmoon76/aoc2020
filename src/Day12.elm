module Day12 exposing (..)

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
        , updateDayModel = \model daymodel -> { model | day12 = daymodel }
        , input = NotAsked
        , inputFile = "day12_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    readDirections input
        |> runDirections
        |> manhattanDistance
        |> Just


part2 : String -> Maybe Int
part2 input =
    Nothing


type CompassPoint
    = North
    | East
    | South
    | West


type Hand
    = Left
    | Right


type Instruction
    = Move CompassPoint
    | Rotate Hand
    | Advance


type alias Direction =
    ( Instruction, Int )


type alias Directions =
    List Direction


type alias Point =
    ( Int, Int )


addPoints : ( number, number ) -> ( number, number ) -> ( number, number )
addPoints x y =
    ( Tuple.first x + Tuple.first y, Tuple.second x + Tuple.second y )

manhattanDistance : Point -> Int
manhattanDistance point =
    abs (Tuple.first point) + abs (Tuple.second point)

type alias State =
    ( CompassPoint, Point )


runDirections : Directions -> Point
runDirections directions =
    runDirectionsHelp directions ( East, ( 0, 0 ) )


runDirectionsHelp : Directions -> State -> Point
runDirectionsHelp directions state =
    case directions of
        x :: xs ->
            runDirectionsHelp xs (runDirection x state)

        _ ->
            Tuple.second state


runDirection : Direction -> State -> State
runDirection direction state =
    let
        facing =
            Tuple.first state

        position =
            Tuple.second state

        move_ : Point -> Int -> Point -> Point
        move_ delta n pos =
            ( Tuple.first pos + n * Tuple.first delta, Tuple.second pos + n * Tuple.second delta )

        move : CompassPoint -> Int -> Point -> Point
        move compassPoint n pos =
            move_ (compassPointToDelta compassPoint) n pos

        compassPointToDelta : CompassPoint -> Point
        compassPointToDelta compassPoint =
            case compassPoint of
                North ->
                    ( 0, 1 )

                East ->
                    ( 1, 0 )

                South ->
                    ( 0, -1 )

                West ->
                    ( -1, 0 )

        compassPointToInt : CompassPoint -> Int
        compassPointToInt compassPoint =
            case compassPoint of
                North ->
                    0

                East ->
                    1

                South ->
                    2

                West ->
                    3

        intToCompassPoint : Int -> CompassPoint
        intToCompassPoint n =
            case modBy 4 n of
                0 ->
                    North

                1 ->
                    East

                2 ->
                    South

                _ ->
                    West

        degreesToInt : Int -> Int
        degreesToInt degrees =
            case degrees of
                90 ->
                    1

                180 ->
                    2

                270 ->
                    3

                _ ->
                    0

        degreesAndHandToInt : Int -> Hand -> Int
        degreesAndHandToInt degrees hand =
            case hand of
                Left ->
                    0 - degreesToInt degrees

                Right ->
                    degreesToInt degrees

        rotate : Hand -> Int -> CompassPoint
        rotate hand degrees =
            degreesAndHandToInt degrees hand
                |> (+) (compassPointToInt facing)
                |> intToCompassPoint
    in
    case direction of
        ( Advance, n ) ->
            ( facing, move facing n position )

        ( Rotate Left, n ) ->
            ( rotate Left n, position )

        ( Rotate Right, n ) ->
            ( rotate Right n, position )

        ( Move p, n ) ->
            ( facing, move p n position )


readDirections : String -> Directions
readDirections input =
    String.split "\n" input
        |> List.filterMap readDirection


readDirection : String -> Maybe Direction
readDirection input =
    let
        instructionList =
            [ ( "N", Move North )
            , ( "E", Move East )
            , ( "S", Move South )
            , ( "W", Move West )
            , ( "L", Rotate Left )
            , ( "R", Rotate Right )
            , ( "F", Advance )
            ]

        instructionChoiceParser : String -> Instruction -> Parser.Parser Instruction
        instructionChoiceParser symbol instruction =
            Parser.symbol symbol |> Parser.map (always instruction)

        instructionParser : Parser.Parser Instruction
        instructionParser =
            Parser.oneOf <|
                List.map (\x -> instructionChoiceParser (Tuple.first x) (Tuple.second x)) instructionList

        directionParser : Parser.Parser Direction
        directionParser =
            Parser.succeed (\x y -> ( x, y ))
                |= instructionParser
                |= Parser.int
                |. Parser.end
    in
    Parser.run directionParser input |> Result.toMaybe


example : String
example =
    """F10
N3
F7
R90
F11"""
