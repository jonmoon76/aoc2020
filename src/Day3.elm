module Day3 exposing (..)

import Array exposing (Array(..))
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


type GridSquare
    = Open
    | Tree


type alias MapRow =
    Array GridSquare


type alias Map =
    Array MapRow


getMapSquare : ( Int, Int ) -> Map -> Maybe GridSquare
getMapSquare pos map =
    let
        getFromRow : MapRow -> Maybe GridSquare
        getFromRow row =
            Array.get (modBy (Array.length row) (Tuple.first pos)) row
    in
    Array.get (Tuple.second pos) map |> Maybe.andThen getFromRow


createMap : String -> Maybe Map
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
                [ Parser.succeed (\x -> Parser.Loop (Open :: rowEntries))
                    |= Parser.symbol "."
                , Parser.succeed (\x -> Parser.Loop (Tree :: rowEntries))
                    |= Parser.symbol "#"
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse rowEntries))
                ]

        createMapRow : String -> Maybe MapRow
        createMapRow rowInput =
            Parser.run rowParser rowInput
                |> Result.toMaybe
                |> Maybe.map Array.fromList

        createMap_ : List (Maybe MapRow) -> Maybe Map
        createMap_ rows =
            if List.member Nothing rows then
                Maybe.Nothing

            else
                Just
                    (List.filterMap identity rows
                        |> Array.fromList
                    )
    in
    input
        |> String.split "\n"
        |> List.filter (not << String.isEmpty)
        |> List.map createMapRow
        |> createMap_


addPoints : ( number, number ) -> ( number, number ) -> ( number, number )
addPoints x y =
    ( Tuple.first x + Tuple.first y, Tuple.second x + Tuple.second y )


countTreesHelper : ( Int, Int ) -> Int -> ( Int, Int ) -> Map -> Int
countTreesHelper pos count delta map =
    let
        recurse increment =
            countTreesHelper (addPoints pos delta) (count + increment) delta map
    in
    case getMapSquare pos map of
        Just Tree ->
            recurse 1

        Just Open ->
            recurse 0

        Nothing ->
            -- Bottom of map - return the result.
            count


countTrees : ( Int, Int ) -> Map -> Int
countTrees delta map =
    countTreesHelper ( 0, 0 ) 0 delta map


countTrees2 : List ( Int, Int ) -> Map -> Int
countTrees2 deltas map =
    List.map (\x -> countTrees x map) deltas
        |> List.foldl (*) 1


updateModel : Model -> WebData String -> Model
updateModel model input =
    { model | day3 = SingleFileModel input }


loadInput : Msg
loadInput =
    Run "day3_input.txt" updateModel


init : SingleFileModel
init =
    { input = NotAsked }


evaluate : (Map -> Int) -> SingleFileModel -> Maybe Int
evaluate evaluator model =
    case model.input of
        Success input ->
            createMap input |> Maybe.map evaluator

        _ ->
            Nothing


part1 : Evaluator
part1 model =
    evaluate (countTrees ( 3, 1 )) model.day3


part2 : Evaluator
part2 model =
    evaluate (countTrees2 [ ( 1, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 1 ), ( 1, 2 ) ]) model.day3
