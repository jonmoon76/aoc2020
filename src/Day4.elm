module Day4 exposing (..)

import Array exposing (Array(..))
import Dict exposing (..)
import Parser exposing ((|.), (|=))
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


good_examples : String
good_examples =
    """
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
"""


bad_examples : String
bad_examples =
    """
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
"""


updateModel : Model -> WebData String -> Model
updateModel model input =
    { model | day4 = SingleFileModel input }


init : SingleFileModel
init =
    { input = NotAsked }


loadInput : Msg
loadInput =
    Run "day4_input.txt" updateModel


type alias Stage1Record =
    Dict String String


ignore : a -> b -> a
ignore a b =
    a


type alias Stage2Record =
    { byr : String
    , iyr : String
    , eyr : String
    , hgt : String
    , hcl : String
    , ecl : String
    , pid : String
    }


type Height
    = Inches Int
    | Cm Int


type EyeColor
    = Amber
    | Blue
    | Brown
    | Grey
    | Green
    | Hazel
    | Other


type alias HairColor =
    String


type alias Stage3Record =
    { byr : Int
    , iyr : Int
    , eyr : Int
    , hgt : Height
    , hcl : HairColor
    , ecl : EyeColor
    , pid : Int
    }


loadStage1Records : String -> List Stage1Record
loadStage1Records input =
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

        recordParser : Parser.Parser Stage1Record
        recordParser =
            Parser.succeed Dict.fromList
                |= Parser.loop [] recordParserHelp

        recordParserHelp : List ( String, String ) -> Parser.Parser (Parser.Step (List ( String, String )) (List ( String, String )))
        recordParserHelp recordDefn =
            Parser.oneOf
                [ Parser.succeed (\x -> Parser.Loop (x :: recordDefn))
                    |= keyValueParser
                , Parser.end
                    |> Parser.map (\_ -> Parser.Done (List.reverse recordDefn))
                , Parser.spaces
                    |> Parser.map (\_ -> Parser.Loop recordDefn)
                ]
    in
    String.split "\n\n" input
        |> List.map (Parser.run recordParser >> Result.toMaybe)
        |> List.filterMap identity


convertStage1ToStage2 : Stage1Record -> Maybe Stage2Record
convertStage1ToStage2 r1 =
    let
        createList =
            List.map (\key -> Dict.get key r1) [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

        --case (Dict.get "byr", Dict.get "iyr", Dict.get "eyr") of
    in
    case createList of
        [ Just byr, Just iyr, Just eyr, Just hgt, Just hcl, Just ecl, Just pid ] ->
            Just (Stage2Record byr iyr eyr hgt hcl ecl pid)

        _ ->
            Maybe.Nothing


checkIntegerRange : Int -> Int -> Int -> a -> Maybe a
checkIntegerRange min max x v =
    if x >= min && x <= max then
        Just v

    else
        Nothing


parseIntegerField : Int -> Int -> Int -> String -> Maybe Int
parseIntegerField numChars min max input =
    checkIntegerRange numChars numChars (String.length input) input
        |> Maybe.andThen String.toInt
        |> Maybe.andThen (\x -> checkIntegerRange min max x x)


parseByr : String -> { a | byr : Int } -> Maybe { a | byr : Int }
parseByr input record =
    parseIntegerField 4 1920 2002 input
        -- |> Debug.log "byr"
        |> Maybe.map (\x -> { record | byr = x })


parseIyr : String -> { a | iyr : Int } -> Maybe { a | iyr : Int }
parseIyr input record =
    parseIntegerField 4 2010 2020 input
        -- |> Debug.log "iyr"
        |> Maybe.map (\x -> { record | iyr = x })


parseEyr : String -> { a | eyr : Int } -> Maybe { a | eyr : Int }
parseEyr input record =
    parseIntegerField 4 2020 2030 input
        -- |> Debug.log "eyr"
        |> Maybe.map (\x -> { record | eyr = x })


parseHeightField : String -> Maybe Height
parseHeightField input =
    let
        heightParser : Parser.Parser Height
        heightParser =
            Parser.succeed (\x y -> y x)
                |= Parser.int
                |= Parser.oneOf
                    [ Parser.map (\_ -> Inches) <| Parser.symbol "in"
                    , Parser.map (\_ -> Cm) <| Parser.symbol "cm"
                    ]
    in
    Parser.run heightParser input |> Result.toMaybe


checkHeight : Height -> Maybe Height
checkHeight h =
    case h of
        Inches a ->
            checkIntegerRange 59 76 a h

        Cm b ->
            checkIntegerRange 150 193 b h


parseHgt : String -> { a | hgt : Height } -> Maybe { a | hgt : Height }
parseHgt input record =
    parseHeightField input
        |> Maybe.andThen checkHeight
        -- |> Debug.log "hgt"
        |> Maybe.map (\x -> { record | hgt = x })


parseHairColorField : String -> Maybe HairColor
parseHairColorField input =
    let
        isValidDigit x =
            Char.isHexDigit x && (Char.isLower x || not (Char.isAlpha x))

        hairColorParser : Parser.Parser String
        hairColorParser =
            Parser.succeed identity
                |. Parser.symbol "#"
                |= Parser.variable { start = isValidDigit, inner = isValidDigit, reserved = Set.empty }

        checkHairColorString : String -> Maybe String
        checkHairColorString s =
            checkIntegerRange 6 6 (String.length s) s
    in
    Parser.run hairColorParser input
        |> Result.toMaybe
        |> Maybe.andThen checkHairColorString
        |> Maybe.map (\x -> input)


parseHcl : String -> { a | hcl : HairColor } -> Maybe { a | hcl : HairColor }
parseHcl input record =
    parseHairColorField input
        -- |> Debug.log "hcl"
        |> Maybe.map (\x -> { record | hcl = x })


parseEyeColorField : String -> Maybe EyeColor
parseEyeColorField input =
    let
        oneEyeColorParser : EyeColor -> String -> Parser.Parser EyeColor
        oneEyeColorParser color symbol =
            Parser.map (\_ -> color) <| Parser.symbol symbol

        eyeColorParser : Parser.Parser EyeColor
        eyeColorParser =
            Parser.oneOf
                [ oneEyeColorParser Amber "amb"
                , oneEyeColorParser Blue "blu"
                , oneEyeColorParser Brown "brn"
                , oneEyeColorParser Grey "gry"
                , oneEyeColorParser Green "grn"
                , oneEyeColorParser Hazel "hzl"
                , oneEyeColorParser Other "oth"
                ]
    in
    Parser.run eyeColorParser input
        |> Result.toMaybe


parseEcl : String -> { a | ecl : EyeColor } -> Maybe { a | ecl : EyeColor }
parseEcl input record =
    parseEyeColorField input
        -- |> Debug.log "ecl"
        |> Maybe.map (\x -> { record | ecl = x })


parsePid : String -> { a | pid : Int } -> Maybe { a | pid : Int }
parsePid input record =
    parseIntegerField 9 0 999999999 input
        -- |> Debug.log "pid"
        |> Maybe.map (\x -> { record | pid = x })


convertStage2ToStage3 : Stage2Record -> Maybe Stage3Record
convertStage2ToStage3 input =
    let
        nullRecord =
            { byr = 0
            , iyr = 0
            , eyr = 0
            , hgt = Inches 0
            , hcl = "x000000"
            , ecl = Brown
            , pid = 0
            }
    in
    Just nullRecord
        |> Maybe.andThen (parseByr input.byr)
        |> Maybe.andThen (parseIyr input.iyr)
        |> Maybe.andThen (parseEyr input.eyr)
        |> Maybe.andThen (parseHgt input.hgt)
        |> Maybe.andThen (parseHcl input.hcl)
        |> Maybe.andThen (parseEcl input.ecl)
        |> Maybe.andThen (parsePid input.pid)


part1 : Evaluator
part1 model =
    evaluate List.length model.day4


part2 : Evaluator
part2 model =
    evaluate (List.map convertStage2ToStage3 >> List.filterMap identity >> List.length) model.day4


evaluate : (List Stage2Record -> Int) -> SingleFileModel -> Maybe Int
evaluate evaluator model =
    case model.input of
        Success input ->
            Just <| (loadStage1Records input |> List.map convertStage1ToStage2 |> List.filterMap identity |> evaluator)

        _ ->
            Nothing
