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
part1 input =
    createMap input
        |> convergeMap
        |> countMap Occupied
        |> Just


part2 : String -> Maybe Int
part2 input =
    Nothing


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
    List.cartesianProduct [ List.range -1 1, List.range -1 1 ]
        |> List.filterMap
            (\p ->
                case p of
                    [ x, y ] ->
                        if x == 0 && y == 0 then
                            Nothing

                        else
                            Just ( x, y )

                    _ ->
                        Nothing
            )


countNeighbors : Point -> Map -> Int
countNeighbors pos map =
    neighborVectors
        |> List.map (addPoints pos)
        |> List.filterMap (getMapSquare map)
        |> List.filterMap
            (\x ->
                if x == Occupied then
                    Just ()

                else
                    Nothing
            )
        |> List.length


updateSquare : Point -> ( Map, Map, Bool ) -> ( Map, Map, Bool )
updateSquare pos mapState =
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
                    if countNeighbors pos oldMap == 0 then
                        ( Occupied, True )

                    else
                        ( Empty, False )

                Just Occupied ->
                    if countNeighbors pos oldMap >= 4 then
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


updateMap : Map -> ( Map, Map, Bool )
updateMap map =
    allCoordinates map
        |> List.foldl updateSquare ( map, map, False )


convergeMap : Map -> Map
convergeMap map =
    let
        convergeMapHelp : Int -> Map -> Map
        convergeMapHelp i map_ =
            case updateMap map_ of
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
