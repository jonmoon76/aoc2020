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
        { example = Just example2
        , updateDayModel = \model daymodel -> { model | day14 = daymodel }
        , input = NotAsked
        , inputFile = "day14_input.txt"
        , part1 = part1
        , part2 = part2
        }


part1 : String -> Maybe Int
part1 =
    partN Version1


part2 : String -> Maybe Int
part2 =
    partN Version2


partN : Version -> String -> Maybe Int
partN version =
    readProgram >> runProgram version >> Just


example : String
example =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""


example2 : String
example2 =
    """mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"""


type Version
    = Version1
    | Version2


type MaskField
    = One
    | Zero
    | Unspecified


type alias Mask =
    List MaskField


type alias Memory =
    Dict Int Int


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
    decomposeHelp n []


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


myBitwise : (a -> Bool -> b) -> List a -> Int -> List b
myBitwise op opArg value =
    let
        decomposed_value =
            decompose value

        normalised_value : List Bool
        normalised_value =
            List.map (\_ -> False) (List.range (List.length decomposed_value + 1) (List.length opArg)) ++ decomposed_value
    in
    List.map2 op opArg normalised_value


applyMaskPart1 : Mask -> Int -> Int
applyMaskPart1 mask value =
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
    myBitwise op mask value |> recompose


applyMaskPart2 : Mask -> Int -> Mask
applyMaskPart2 mask address =
    let
        op : MaskField -> Bool -> MaskField
        op maskField bit =
            case maskField of
                One ->
                    One

                Zero ->
                    case bit of
                        True ->
                            One

                        False ->
                            Zero

                Unspecified ->
                    Unspecified
    in
    myBitwise op mask address


expandMask : Mask -> List (List Bool)
expandMask mask =
    let
        expandMaskHelp : List Bool -> Mask -> List (List Bool)
        expandMaskHelp prefix remainingMask =
            case remainingMask of
                [] ->
                    [ prefix ]

                maskField :: newRemainingMask ->
                    case maskField of
                        Zero ->
                            expandMaskHelp (False :: prefix) newRemainingMask

                        One ->
                            expandMaskHelp (True :: prefix) newRemainingMask

                        Unspecified ->
                            expandMaskHelp (False :: prefix) newRemainingMask
                                ++ expandMaskHelp (True :: prefix) newRemainingMask
    in
    expandMaskHelp [] mask
        |> List.map List.reverse


runProgram : Version -> Program -> Int
runProgram version program =
    let
        setMemVersion1 : Int -> Int -> State -> State
        setMemVersion1 address value state =
            { state | memory = Dict.insert address (applyMaskPart1 state.mask value) state.memory }

        expandAddresses : Mask -> Int -> List Int
        expandAddresses mask address =
            applyMaskPart2 mask address
                |> expandMask
                |> List.map recompose

        updateMemory : Int -> Mask -> Int -> Memory -> Memory
        updateMemory value mask address oldMemory =
            List.foldl (\a memory -> Dict.insert a value memory) oldMemory <| expandAddresses mask address

        setMemVersion2 : Int -> Int -> State -> State
        setMemVersion2 address value state =
            { state | memory = updateMemory value state.mask address state.memory }

        runInstruction : Instruction -> State -> State
        runInstruction instruction state =
            case instruction of
                SetMask mask ->
                    { state | mask = mask }

                SetMem address value ->
                    case version of
                        Version1 ->
                            setMemVersion1 address value state

                        Version2 ->
                            setMemVersion2 address value state

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
