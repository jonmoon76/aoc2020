module Day11 exposing (..)

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


init : DayModel
init =
    DayModel
        { example = Just example
        , updateDayModel = \model daymodel -> { model | day11 = daymodel }
        , input = NotAsked
        , inputFile = "day11_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 =
    partHelp 4 checkDirectionImmediate


part2 : String -> Maybe Int
part2 =
    partHelp 5 checkDirectionFar


partHelp : Int -> Checker -> String -> Maybe Int
partHelp occupiedBoundary checker input =
    createMap input
        |> convergeMap occupiedBoundary checker
        |> countMap Occupied
        |> Just


type GridSquare
    = Floor
    | Empty
    | Occupied


type alias MapRow =
    Array GridSquare


type alias Map =
    Array MapRow


type alias Point =
    ( Int, Int )


type alias Checker =
    Map -> Point -> Point -> Maybe ()


getMapSquare : Map -> Point -> Maybe GridSquare
getMapSquare map pos =
    Array.get (Tuple.second pos) map |> Maybe.andThen (Array.get (Tuple.first pos))


setMapSquare : GridSquare -> Point -> Map -> Map
setMapSquare value pos map =
    let
        oldRow =
            Array.get (Tuple.second pos) map |> Maybe.withDefault Array.empty

        newRow =
            Array.set (Tuple.first pos) value oldRow
    in
    Array.set (Tuple.second pos) newRow map


addPoints : ( number, number ) -> ( number, number ) -> ( number, number )
addPoints x y =
    ( Tuple.first x + Tuple.first y, Tuple.second x + Tuple.second y )


neighborVectors : List Point
neighborVectors =
    [ ( -1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( -1, 1 ) ]


checkDirectionImmediate : Map -> Point -> Point -> Maybe ()
checkDirectionImmediate map pos delta =
    case getMapSquare map pos of
        Just Occupied ->
            Just ()

        _ ->
            Nothing


checkDirectionFar : Map -> Point -> Point -> Maybe ()
checkDirectionFar map pos delta =
    case getMapSquare map pos of
        Just Occupied ->
            Just ()

        Just Empty ->
            Nothing

        Nothing ->
            Nothing

        _ ->
            checkDirectionFar map (addPoints pos delta) delta


countNeighbors : Checker -> Point -> Map -> Int
countNeighbors checker pos map =
    neighborVectors
        |> List.filterMap (\d -> checker map (addPoints pos d) d)
        |> List.length


updateSquare : Int -> Checker -> Point -> ( Map, Map, Bool ) -> ( Map, Map, Bool )
updateSquare occupiedBoundary checker pos mapState =
    let
        newMap =
            case mapState of
                ( _, n, _ ) ->
                    n

        oldMap =
            case mapState of
                ( n, _, _ ) ->
                    n

        newGridValue =
            case getMapSquare oldMap pos of
                Just Empty ->
                    if countNeighbors checker pos oldMap == 0 then
                        ( Occupied, True )

                    else
                        ( Empty, False )

                Just Occupied ->
                    if countNeighbors checker pos oldMap >= occupiedBoundary then
                        ( Empty, True )

                    else
                        ( Occupied, False )

                _ ->
                    ( Floor, False )
    in
    case newGridValue of
        ( v, True ) ->
            ( oldMap, setMapSquare v pos newMap, True )

        _ ->
            mapState


allCoordinates : Map -> List Point
allCoordinates map =
    let
        mapHeight =
            Array.length map

        mapWidth =
            Array.length (Array.get 0 map |> Maybe.withDefault Array.empty)

        axis max =
            List.range 0 (max - 1)

        listToPoint list =
            case list of
                x :: y :: [] ->
                    Just ( x, y )

                _ ->
                    Nothing
    in
    List.cartesianProduct [ axis mapWidth, axis mapHeight ]
        |> List.filterMap listToPoint


updateMap : Int -> Checker -> Map -> ( Map, Map, Bool )
updateMap occupiedBoundary checker map =
    allCoordinates map
        |> List.foldl (updateSquare occupiedBoundary checker) ( map, map, False )


convergeMap : Int -> Checker -> Map -> Map
convergeMap occupiedBoundary checker map =
    let
        convergeMapHelp : Int -> Map -> Map
        convergeMapHelp i map_ =
            case updateMap occupiedBoundary checker map_ of
                ( _, m, False ) ->
                    m

                ( _, m, True ) ->
                    convergeMapHelp (i + 1) m
    in
    convergeMapHelp 1 map


countMap : GridSquare -> Map -> Int
countMap value map =
    allCoordinates map
        |> List.filterMap
            (\point ->
                if getMapSquare map point == Just value then
                    Just value

                else
                    Nothing
            )
        |> List.length


createMap : String -> Map
createMap input =
    let
        rowParser : Parser.Parser (List GridSquare)
        rowParser =
            Parser.succeed identity
                |= Parser.loop [] rowParserHelp
                |. Parser.end

        rowParserHelp : List GridSquare -> Parser.Parser (Parser.Step (List GridSquare) (List GridSquare))
        rowParserHelp rowEntries =
            Parser.oneOf
                [ Parser.symbol "." |> Parser.map (\_ -> Parser.Loop (Floor :: rowEntries))
                , Parser.symbol "L" |> Parser.map (\_ -> Parser.Loop (Empty :: rowEntries))
                , Parser.symbol "#" |> Parser.map (\_ -> Parser.Loop (Occupied :: rowEntries))
                , Parser.succeed () |> Parser.map (\_ -> Parser.Done (List.reverse rowEntries))
                ]

        createMapRow : String -> Maybe MapRow
        createMapRow rowInput =
            Parser.run rowParser rowInput
                |> Result.toMaybe
                |> Maybe.map Array.fromList
    in
    input
        |> String.split "\n"
        |> List.filter (not << String.isEmpty)
        |> List.filterMap createMapRow
        |> Array.fromList


gridToString : GridSquare -> String
gridToString grid =
    case grid of
        Occupied ->
            "#"

        Floor ->
            "."

        Empty ->
            "L"


mapToString : Map -> String
mapToString map =
    Array.toList map
        |> List.map (Array.toList >> List.map gridToString >> String.join "")
        |> String.join "\n"


example : String
example =
    """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""
