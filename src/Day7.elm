module Day7 exposing (..)

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
        , updateDayModel = \model daymodel -> { model | day7 = daymodel }
        , input = NotAsked
        , inputFile = "day7_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    doPart (countContainedBags "shiny gold") input


part2 : String -> Maybe Int
part2 input =
    doPart (countContainingBags "shiny gold") input


doPart : (List Rule -> Int) -> String -> Maybe Int
doPart evaluator input =
    String.split "\n" input
        |> List.filterMap parseRule
        -- Subtract one for the starting bag
        |> (evaluator >> (\x -> x - 1))
        |> Just


example =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""


type alias ColorString =
    String


type alias Content =
    { color : ColorString
    , count : Int
    }


type alias Rule =
    { color : ColorString
    , contents : List Content
    }


parseRule : String -> Maybe Rule
parseRule input =
    let
        wordParser : Parser.Parser String
        wordParser =
            Parser.variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }

        colorParser : Parser.Parser ColorString
        colorParser =
            Parser.succeed (\a b -> a ++ " " ++ b)
                |= wordParser
                |. Parser.symbol " "
                |= wordParser

        contentParser : Parser.Parser Content
        contentParser =
            Parser.succeed (\count color -> Content color count)
                |= Parser.int
                |. Parser.symbol " "
                |= colorParser
                |. Parser.oneOf
                    [ Parser.symbol " bags"
                    , Parser.symbol " bag"
                    ]
                |. Parser.oneOf
                    [ Parser.symbol ", "
                    , Parser.symbol "."
                    ]

        contentsParser : Parser.Parser (List Content)
        contentsParser =
            Parser.oneOf
                [ Parser.symbol "no other bags." |> Parser.map (\x -> [])
                , Parser.loop [] contentsParserHelp
                ]

        contentsParserHelp : List Content -> Parser.Parser (Parser.Step (List Content) (List Content))
        contentsParserHelp contents =
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: contents))
                    |= contentParser
                , Parser.end
                    |> Parser.map (\_ -> Parser.Done (List.reverse contents))
                ]

        ruleParser : Parser.Parser Rule
        ruleParser =
            Parser.succeed Rule
                |= colorParser
                |. Parser.symbol " bags contain "
                |= contentsParser
                |. Parser.end
    in
    --Result.toMaybe (Debug.log "In" input |> Parser.run ruleParser |> Debug.log "r")
    Result.toMaybe (Parser.run ruleParser input)


countContainedBags : ColorString -> List Rule -> Int
countContainedBags color rules =
    countContainedBagsHelp rules ( Set.singleton color, Set.empty )
        |> Set.size


countContainedBagsHelp : List Rule -> ( Set ColorString, Set ColorString ) -> Set ColorString
countContainedBagsHelp rules state =
    let
        findRulesContaining : Set ColorString -> Set ColorString
        findRulesContaining colors =
            let
                ruleContainsColors : Rule -> Set ColorString
                ruleContainsColors rule =
                    List.map (\c -> c.color) rule.contents
                        |> Set.fromList

                isRuleRelevant : Rule -> Bool
                isRuleRelevant rule =
                    Set.intersect colors (ruleContainsColors rule)
                        |> Set.isEmpty
                        |> not
            in
            List.filterMap
                (\rule ->
                    if isRuleRelevant rule then
                        Just rule.color

                    else
                        Nothing
                )
                rules
                |> Set.fromList

        -- Add elements of seed to results
        newResults : Set ColorString -> Set ColorString -> Set ColorString
        newResults oldSeed oldResults =
            Set.union oldSeed oldResults

        newSeed : Set ColorString -> Set ColorString -> Set ColorString
        newSeed results candidateNewSeed =
            List.foldl
                (\color seed ->
                    if Set.member color results then
                        seed

                    else
                        Set.insert color seed
                )
                Set.empty
                (Set.toList candidateNewSeed)

        newState : Set ColorString -> Set ColorString -> Set ColorString -> ( Set ColorString, Set ColorString )
        newState oldSeed oldResults candidateNewSeed =
            ( newSeed (newResults oldSeed oldResults) candidateNewSeed, newResults oldSeed oldResults )
    in
    case state of
        ( seed, results ) ->
            if Set.isEmpty seed then
                results

            else
                findRulesContaining seed
                    |> newState seed results
                    |> countContainedBagsHelp rules


countContainingBags : ColorString -> List Rule -> Int
countContainingBags color rules =
    let
        rulesDict : Dict ColorString Rule
        rulesDict =
            List.map (\x -> ( x.color, x )) rules
                |> Dict.fromList
    in
    countContainingBagsHelp color rulesDict


countContainingBagsHelp : ColorString -> Dict ColorString Rule -> Int
countContainingBagsHelp color rules =
    let
        count : Rule -> Int
        count rule =
            List.map (\c -> countContainingBagsHelp c.color rules * c.count) rule.contents
                |> List.sum
                |> (+) 1
    in
    Dict.get color rules
        |> Maybe.map count
        |> Maybe.withDefault 0
