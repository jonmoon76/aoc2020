module Day16 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Char
import Dict exposing (..)
import Html.Attributes exposing (name)
import Json.Decode exposing (field)
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
        { example = Just example2
        , updateDayModel = \model daymodel -> { model | day8 = daymodel }
        , input = NotAsked
        , inputFile = "day16_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    let
        notes =
            readNotes input
    in
    invalidValuesInTickets notes.fields notes.nearbyTickets
        |> List.sum
        |> Just


part2 : String -> Maybe Int
part2 input =
    let
        notes =
            readNotes input
    in
    List.filter (validTicket notes.fields) notes.nearbyTickets
        |> List.transpose
        |> List.map (candidateFields notes.fields)
        |> solve
        |> List.map2
            (\value mField ->
                case mField of
                    Just field ->
                        if String.startsWith "departure" field then
                            Just value

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
            notes.myTicket
        |> List.filterMap identity
        |> List.product
        |> Just


candidateFields : List Field -> List Int -> List String
candidateFields fields values =
    List.filter (checkValuesAgainstField values)
        fields
        |> List.map .name


type alias Tent =
    List String


type alias Candidates =
    List String


solve : List Candidates -> List (Maybe String)
solve startState =
    let
        isSolved : List String -> Bool
        isSolved possibleFields =
            case possibleFields of
                [ field ] ->
                    True

                _ ->
                    False

        extractSolved : List String -> Maybe String
        extractSolved possibleFields =
            case possibleFields of
                [ field ] ->
                    Just field

                _ ->
                    Nothing

        initialTent : List String
        initialTent =
            List.filterMap extractSolved startState

        removeFromTent : String -> Tent -> List Candidates -> ( List Candidates, Tent )
        removeFromTent field tent currentState =
            let
                updateCandidates : Candidates -> Candidates
                updateCandidates candidates =
                    Set.fromList candidates |> Set.remove field |> Set.toList

                processCandidates : Candidates -> ( List Candidates, Tent ) -> ( List Candidates, Tent )
                processCandidates candidates ( newState, newTent ) =
                    if isSolved candidates then
                        ( candidates :: newState, newTent )

                    else
                        case updateCandidates candidates of
                            [ f ] ->
                                ( [ f ] :: newState, f :: newTent )

                            newCandidates ->
                                ( newCandidates :: newState, newTent )
            in
            List.foldr processCandidates ( [], tent ) currentState

        iterate : ( List Candidates, Tent ) -> List Candidates
        iterate ( currentState, tent ) =
            case tent of
                t :: ts ->
                    iterate <| removeFromTent t ts currentState

                _ ->
                    currentState

        extract : List Candidates -> List (Maybe String)
        extract finalState =
            List.map extractSolved finalState
    in
    iterate ( startState, initialTent )
        |> extract


example : String
example =
    """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"""


example2 : String
example2 =
    """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"""


type alias Range =
    ( Int, Int )


type alias Field =
    { name : String
    , constraint : ( Range, Range )
    }


type alias Ticket =
    List Int


type alias Notes =
    { fields : List Field
    , myTicket : Ticket
    , nearbyTickets : List Ticket
    }


checkFieldsAgainstValue : List Field -> Int -> Bool
checkFieldsAgainstValue constraints value =
    List.foldl (checkValueAGainstField value >> (||)) False constraints


checkValueAGainstField : Int -> Field -> Bool
checkValueAGainstField value field =
    case field.constraint of
        ( ( a, b ), ( c, d ) ) ->
            (value >= a && value <= b) || (value >= c && value <= d)


checkValuesAgainstField : List Int -> Field -> Bool
checkValuesAgainstField values field =
    List.foldl (\value result -> result && checkValueAGainstField value field) True values


invalidValuesInTicket : List Field -> Ticket -> List Int
invalidValuesInTicket fields ticket =
    List.filter (checkFieldsAgainstValue fields >> not) ticket


invalidValuesInTickets : List Field -> List Ticket -> List Int
invalidValuesInTickets fields tickets =
    List.concatMap (invalidValuesInTicket fields) tickets


validTicket : List Field -> Ticket -> Bool
validTicket fields ticket =
    invalidValuesInTicket fields ticket |> List.isEmpty



{-
   INPUT PARSING
-}


readNotes : String -> Notes
readNotes input =
    case String.split "\n\n" input of
        [ constraints, myTicket, nearbyTickets ] ->
            Notes (readFields constraints) (readMyTicket myTicket) (readNearbyTickets nearbyTickets)

        _ ->
            Debug.todo "Invalid notes"


readFields : String -> List Field
readFields input =
    String.split "\n" input |> List.filterMap readField


readField : String -> Maybe Field
readField input =
    let
        rangeParser =
            Parser.succeed Tuple.pair
                |= Parser.int
                |. Parser.symbol "-"
                |= Parser.int

        nameParser =
            Parser.variable { start = Char.isAlpha, inner = \x -> Char.isAlpha x || (x == ' '), reserved = Set.empty }

        fieldParser =
            Parser.succeed (\name c1 c2 -> Field name ( c1, c2 ))
                |= nameParser
                |. Parser.symbol ": "
                |= rangeParser
                |. Parser.symbol " or "
                |= rangeParser
                |. Parser.end
    in
    Parser.run fieldParser input
        |> Result.toMaybe


readMyTicket : String -> Ticket
readMyTicket input =
    case String.split "\n" input of
        [ "your ticket:", ticket ] ->
            readTicket ticket

        _ ->
            Debug.todo "Invalid my ticket input"


readNearbyTickets : String -> List Ticket
readNearbyTickets input =
    case String.split "\n" input of
        "nearby tickets:" :: tickets ->
            List.map readTicket tickets

        _ ->
            Debug.todo "Invalid nearby tickets input"


readTicket : String -> Ticket
readTicket input =
    String.split "," input |> List.filterMap String.toInt
