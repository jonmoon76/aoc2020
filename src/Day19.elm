module Day19 exposing (..)

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
import Utils exposing (..)


init : DayModel
init =
    DayModel
        { example = Just example
        , updateDayModel = \model daymodel -> { model | day19 = daymodel }
        , input = NotAsked
        , inputFile = "day19_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    case readInput input of
        ( rule, messages ) ->
            List.filterMap (checkMessage rule) messages
                |> List.length
                |> Just


part2 : String -> Maybe Int
part2 input =
    Nothing


checkMessage : Rule -> String -> Maybe ()
checkMessage rule message =
    Parser.run rule message |> Debug.log message |> Result.toMaybe


example : String
example =
    """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"""


example2 =
    """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb"""


type alias Rule =
    Parser.Parser ()


type alias RuleDB =
    Dict Int Rule


readInput : String -> ( Rule, List String )
readInput input =
    case String.split "\n\n" input of
        [ rules, messages ] ->
            ( readRules rules, readMessages messages )

        _ ->
            ( Parser.problem "Bad input", [] )


readMessages : String -> List String
readMessages =
    String.split "\n"


readRules : String -> Rule
readRules input =
    String.split "\n" input
        |> List.foldr readRule Dict.empty
        |> Debug.log "Rules"
        |> Dict.get 0
        |> Maybe.withDefault (Parser.problem "No rule 0")
        |> Parser.andThen (\_ -> Parser.end)


readRule : String -> RuleDB -> RuleDB
readRule input rules =
    let
        charParser =
            Parser.succeed Parser.symbol
                |. Parser.symbol " \""
                |= Parser.variable { start = Char.isLower, inner = \_ -> False, reserved = Set.empty }
                |. Parser.symbol "\""

        sequenceParser =
            Parser.succeed (List.filterMap identity >> List.foldl (\p acc -> acc |. p) (Parser.succeed ()))
                |= Parser.loop [] sequenceParserHelp

        sequenceParserHelp state =
            Parser.oneOf
                [ Parser.backtrackable <|
                    Parser.succeed (\n -> Parser.Loop (Dict.get n rules :: state))
                        |. Parser.spaces
                        |= Parser.int
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse state))
                ]

        orParser =
            Parser.succeed (\a b -> Parser.oneOf [ Parser.backtrackable a, b ])
                |= sequenceParser
                |. Parser.symbol " |"
                |= sequenceParser

        ruleParser : Parser.Parser ( Int, Rule )
        ruleParser =
            Parser.succeed (\n r -> ( n, r ))
                |= Parser.int
                |. Parser.symbol ":"
                |= Parser.oneOf
                    [ charParser
                    , Parser.backtrackable orParser
                    , sequenceParser
                    ]
                |. Parser.end
    in
    case Parser.run ruleParser input of
        Err s ->
            Debug.log input s |> always rules

        Ok ( n, p ) ->
            Dict.insert n p rules
