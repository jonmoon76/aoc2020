module Day14 exposing (..)

import Array exposing (Array(..))
import Bitwise exposing (..)
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
        , updateDayModel = \model daymodel -> { model | day14 = daymodel }
        , input = NotAsked
        , inputFile = "day14_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 input =
    readProgram input
        |> (runProgram >> Just)


part2 : String -> Maybe Int
part2 input =
    Nothing


example : String
example =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""


type MaskField
    = One
    | Zero
    | Unspecified


type alias Mask =
    List MaskField


type Instruction
    = SetMask Mask
    | SetMem Int Int


type alias Program =
    List Instruction


type alias State =
    { mask : Mask
    , memory : Dict Int Int
    }


decompose : Int -> List Bool
decompose n =
    let
        getBit m =
            modBy 2 m == 1

        decomposeHelp : Int -> List Bool -> List Bool
        decomposeHelp m values =
            if m == 0 then
                values

            else
                decomposeHelp (floor (toFloat m / 2)) (getBit m :: values)
    in
    trace "decompose" n (\_ -> decomposeHelp n [])


recompose : List Bool -> Int
recompose bits =
    let
        bitToDigit : Bool -> Int
        bitToDigit bit =
            if bit then
                1

            else
                0
    in
    List.foldl (\bit value -> (value * 2) + bitToDigit bit) 0 bits


recomposeTrace : List Bool -> Int
recomposeTrace bits =
    trace "recompose" bits (\_ -> recompose bits)


logValue : String -> a -> a
logValue str a =
    Debug.log str (Debug.toString a) |> always a


myBitwise : (a -> Bool -> Bool) -> List a -> Int -> Int
myBitwise op opArg value =
    let
        decomposed_value =
            decompose value

        normalised_value : List Bool
        normalised_value =
            List.map (\_ -> False) (List.range (List.length decomposed_value + 1) (List.length opArg)) ++ decomposed_value
    in
    List.map2 op opArg normalised_value |> recompose


applyMask : Mask -> Int -> Int
applyMask mask value =
    let
        op : MaskField -> Bool -> Bool
        op maskField bit =
            case maskField of
                One ->
                    True

                Zero ->
                    False

                Unspecified ->
                    bit
    in
    myBitwise op mask value


runProgram : Program -> Int
runProgram program =
    let
        runInstruction : Instruction -> State -> State
        runInstruction instruction state =
            case instruction of
                SetMask mask ->
                    { state | mask = mask }

                SetMem address value ->
                    { state | memory = Dict.insert address (applyMask state.mask value) state.memory }

        evaluateMemory : State -> Int
        evaluateMemory state =
            List.sum <| Dict.values state.memory
    in
    List.foldl runInstruction { mask = [], memory = Dict.empty } program
        |> evaluateMemory


readProgram : String -> Program
readProgram input =
    let
        maskLineParser =
            Parser.succeed (\x -> SetMask (readMask x))
                |. Parser.symbol "mask = "
                |= Parser.getChompedString (Parser.chompWhile (\_ -> True))
                |. Parser.end

        setLineParser =
            Parser.succeed SetMem
                |. Parser.symbol "mem["
                |= Parser.int
                |. Parser.symbol "] = "
                |= Parser.int
                |. Parser.end

        lineParser =
            Parser.oneOf
                [ maskLineParser
                , setLineParser
                ]

        readLine : String -> Maybe Instruction
        readLine line =
            Parser.run lineParser line |> Result.toMaybe

        readLineTrace line =
            trace "readLine" line (\_ -> readLine line)
    in
    String.split "\n" input
        |> List.filterMap readLine


readMask : String -> Mask
readMask maskString =
    let
        readField : Char -> MaskField
        readField c =
            case c of
                '1' ->
                    One

                '0' ->
                    Zero

                _ ->
                    Unspecified

        processField : Char -> Mask -> Mask
        processField c mask =
            readField c :: mask
    in
    String.toList maskString
        |> List.foldr processField []
