module Day12 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
import Html exposing (a)
import Matrix exposing (Matrix(..))
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
part1 =
    part Basic ( 1, 0 )


part2 : String -> Maybe Int
part2 =
    part Waypoint ( 10, 1 )


part : Mode -> Point -> String -> Maybe Int
part mode waypoint input =
    readDirections input
        |> runDirections mode waypoint
        |> manhattanDistance
        |> Just


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


type Mode
    = Basic
    | Waypoint


type alias Direction =
    ( Instruction, Int )


type alias Directions =
    List Direction


type alias Point =
    ( Int, Int )


type alias State =
    { mode : Mode
    , waypoint : Point
    , position : Point
    }


rotationMatrix : Int -> Matrix Int
rotationMatrix angle =
    let
        floatAngle =
            degrees (toFloat angle)

        element c =
            case c of
                ( 1, 1 ) ->
                    cos floatAngle

                ( 1, 2 ) ->
                    sin floatAngle

                ( 2, 1 ) ->
                    negate (sin floatAngle)

                ( 2, 2 ) ->
                    cos floatAngle

                _ ->
                    0
    in
    Matrix.initialize 2 2 element
        |> Matrix.map round


rotatePoint : Int -> Point -> Point
rotatePoint degrees point =
    let
        pointToMatrix =
            Matrix.fromList 2 1 [ Tuple.first point, Tuple.second point ]

        matrixToPoint matrix =
            ( Matrix.get 1 1 matrix, Matrix.get 2 1 matrix )
                |> Tuple.mapBoth (Maybe.withDefault 0) (Maybe.withDefault 0)
    in
    pointToMatrix
        |> Maybe.andThen (Matrix.dot (rotationMatrix degrees))
        |> Maybe.map matrixToPoint
        |> Maybe.withDefault ( 0, 0 )


adjustAngle : Hand -> Int -> Int
adjustAngle hand angle =
    case hand of
        Left ->
            negate angle

        Right ->
            angle


manhattanDistance : Point -> Int
manhattanDistance point =
    abs (Tuple.first point) + abs (Tuple.second point)


mapPoint : Point -> Int -> Point -> Point
mapPoint delta n pos =
    ( Tuple.first pos + n * Tuple.first delta, Tuple.second pos + n * Tuple.second delta )


mapPointByCompassPoint : CompassPoint -> Int -> Point -> Point
mapPointByCompassPoint compassPoint n pos =
    mapPoint (compassPointToDelta compassPoint) n pos


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


runDirections : Mode -> Point -> Directions -> Point
runDirections mode waypoint directions =
    runDirectionsHelp directions (State mode waypoint ( 0, 0 ))


runDirectionsHelp : Directions -> State -> Point
runDirectionsHelp directions state =
    case directions of
        x :: xs ->
            runDirectionsHelp xs (runDirection x state)

        _ ->
            state.position


runDirection : Direction -> State -> State
runDirection direction state =
    let
        mode =
            state.mode

        waypoint =
            state.waypoint

        position =
            state.position
    in
    case direction of
        ( Advance, n ) ->
            State mode waypoint (mapPoint waypoint n position)

        ( Rotate hand, angle ) ->
            State mode (rotatePoint (adjustAngle hand angle) waypoint) position

        ( Move compassPoint, n ) ->
            case mode of
                Basic ->
                    State mode waypoint (mapPointByCompassPoint compassPoint n position)

                Waypoint ->
                    State mode (mapPointByCompassPoint compassPoint n waypoint) position


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
