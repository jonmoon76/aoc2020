module Day5 exposing (..)

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
        , updateDayModel = \model daymodel -> { model | day5 = daymodel }
        , input = NotAsked
        , inputFile = "day5_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 =
    loadRecords >> List.map seatID >> List.maximum


part2 : String -> Maybe Int
part2 =
    let
        checkMissing : Int -> ( Maybe Int, Maybe Int ) -> ( Maybe Int, Maybe Int )
        checkMissing id state =
            case state of
                ( Nothing, result ) ->
                    ( Just id, Nothing )

                ( Just prevID, result ) ->
                    ( Just id
                    , if prevID + 1 == id then
                        result

                      else
                        Just (prevID + 1)
                    )
    in
    loadRecords >> List.map seatID >> List.sort >> List.foldl checkMissing ( Nothing, Nothing ) >> Tuple.second


type RowIndicator
    = Front
    | Back


type ColumnIndicator
    = Left
    | Right


type alias Record =
    { rowIndicators : List RowIndicator
    , columnIndicators : List ColumnIndicator
    }


loadRecords : String -> List Record
loadRecords input =
    String.split "\n" input
        |> List.map loadRecord
        |> List.filterMap identity


loadRecord : String -> Maybe Record
loadRecord input =
    let
        symbolParser : String -> a -> Parser.Parser a
        symbolParser a t =
            Parser.symbol a |> Parser.map (\x -> t)

        frontBackParser =
            Parser.oneOf
                [ symbolParser "F" Front
                , symbolParser "B" Back
                ]

        leftRightParser =
            Parser.oneOf
                [ symbolParser "L" Left
                , symbolParser "R" Right
                ]

        recordParser =
            Parser.succeed (\r1 r2 r3 r4 r5 r6 r7 c1 c2 c3 -> Record [ r1, r2, r3, r4, r5, r6, r7 ] [ c1, c2, c3 ])
                |= frontBackParser
                |= frontBackParser
                |= frontBackParser
                |= frontBackParser
                |= frontBackParser
                |= frontBackParser
                |= frontBackParser
                |= leftRightParser
                |= leftRightParser
                |= leftRightParser
                |. Parser.end
    in
    Parser.run recordParser input |> Result.toMaybe


seatID : Record -> Int
seatID record =
    8 * rowID record.rowIndicators + columnID record.columnIndicators


rowID : List RowIndicator -> Basics.Int
rowID =
    let
        mapper : RowIndicator -> Int
        mapper r =
            case r of
                Front ->
                    0

                Back ->
                    1
    in
    componentID mapper


columnID : List ColumnIndicator -> Basics.Int
columnID =
    let
        mapper : ColumnIndicator -> Int
        mapper r =
            case r of
                Left ->
                    0

                Right ->
                    1
    in
    componentID mapper


componentID : (a -> Int) -> List a -> Int
componentID f xs =
    let
        component : Int -> ( Int, Int ) -> ( Int, Int )
        component indicator state =
            case state of
                ( exp, acc ) ->
                    ( exp - 1, acc + (2 ^ exp) * indicator )
    in
    List.map f xs
        |> List.foldl component ( List.length xs - 1, 0 )
        |> Tuple.second
