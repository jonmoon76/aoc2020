module Day4 exposing (..)

import Array exposing (Array(..))
import Dict exposing (..)
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


updateModel : Model -> WebData String -> Model
updateModel model input =
    { model | day4 = SingleFileModel input }


init : SingleFileModel
init =
    { input = NotAsked }


loadInput : Msg
loadInput =
    Run "day4_input.txt" updateModel


type alias Record =
    Dict String String


ignore : a -> b -> a
ignore a b =
    a


loadRecords : String -> Maybe (List Record)
loadRecords input =
    let
        keyParser : Parser.Parser String
        keyParser =
            Parser.variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }

        isValidValueChar : Char -> Bool
        isValidValueChar c =
            Set.member c (Set.fromList [ ' ', '\t', '\n' ]) |> not

        valueParser : Parser.Parser String
        valueParser =
            Parser.variable { start = isValidValueChar, inner = isValidValueChar, reserved = Set.empty }

        keyValueParser : Parser.Parser ( String, String )
        keyValueParser =
            Parser.succeed Tuple.pair
                |= keyParser
                |. Parser.symbol ":"
                |= valueParser

        recordParser : Parser.Parser Record
        recordParser =
            Parser.succeed Dict.fromList
                |= Parser.loop [] recordParserHelp

        recordParserHelp : List ( String, String ) -> Parser.Parser (Parser.Step (List ( String, String )) (List ( String, String )))
        recordParserHelp recordDefn =
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: recordDefn))
                    |= keyValueParser
                , Parser.symbol "\n\n"
                    |> Parser.map (\_ -> Parser.Done (List.reverse recordDefn))
                , Parser.end
                    |> Parser.map (\_ -> Parser.Done (List.reverse recordDefn))
                , Parser.spaces
                    |> Parser.map (\_ -> Parser.Loop recordDefn)
                ]

        recordsParser : Parser.Parser (List Record)
        recordsParser =
            Parser.succeed identity
                |= Parser.loop [] recordsParserHelp

        recordsParserHelp : List Record -> Parser.Parser (Parser.Step (List Record) (List Record))
        recordsParserHelp records =
            Parser.oneOf
                [ Parser.end
                    |> Parser.map (\_ -> Parser.Done (List.reverse records))
                , Parser.succeed (\x -> Parser.Loop (x :: records))
                    |= recordParser
                ]
    in
    Parser.run recordsParser input
        |> Result.toMaybe


isRecordValid : Record -> Bool
isRecordValid record =
    List.foldl (\a b -> Dict.member a record && b) True [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]


part1 : Evaluator
part1 model =
    evaluate part1Evaluator model.day4


part1Evaluator : List Record -> Int
part1Evaluator records =
    List.map isRecordValid records
        |> List.map
            (\b ->
                if b then
                    1

                else
                    0
            )
        |> List.sum


part2 : Evaluator
part2 model =
    evaluate (\x -> 0) model.day4


evaluate : (List Record -> Int) -> SingleFileModel -> Maybe Int
evaluate evaluator model =
    case model.input of
        Success input ->
            loadRecords input |> Debug.log "Records:" |> Maybe.map evaluator

        _ ->
            Nothing
