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
    { onesMask : Int, zeroesMask : Int }


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


myBitwise : (Bool -> Bool -> Bool) -> Int -> Int -> Int
myBitwise op a b =
    let
        normalise x y =
            let
                decomposed_x =
                    decompose x

                decomposed_y =
                    decompose y
            in
            List.map (\_ -> False) (List.range (List.length decomposed_y + 1) (List.length decomposed_x)) ++ decomposed_y

        decomposed_a =
            normalise b a

        decomposed_b =
            normalise a b
    in
    ()
        -- ( logValue "a" decomposed_a, logValue "b" decomposed_b )
        |> always (List.map2 op decomposed_a decomposed_b |> recomposeTrace)


runProgram : Program -> Int
runProgram program =
    let
        setValue : Mask -> Int -> Int
        setValue mask inputValue =
            inputValue
                |> myBitwise
                    (\x y ->
                        if x then
                            True

                        else
                            y
                    )
                    mask.onesMask
                |> myBitwise
                    (\x y ->
                        if x then
                            False

                        else
                            y
                    )
                    mask.zeroesMask

        setValueTrace : Int -> Mask -> Int -> Int
        setValueTrace address mask inputValue =
            trace ("set[" ++ String.fromInt address ++ "]") ( mask, inputValue ) (\_ -> setValue mask inputValue)

        runInstruction : Instruction -> State -> State
        runInstruction instruction state =
            case instruction of
                SetMask mask ->
                    { state | mask = mask }

                SetMem address value ->
                    { state | memory = Dict.insert address (setValueTrace address state.mask value) state.memory }

        evaluateMemory : State -> Int
        evaluateMemory state =
            List.sum <| Dict.values state.memory
    in
    List.foldl runInstruction { mask = Mask 0 0, memory = Dict.empty } program
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

        checkValue expressionStr value =
            (if value < 0 then
                Debug.log "!!!" expressionStr

             else
                ""
            )
                |> always value

        checkOr : Int -> Int -> Int
        checkOr a b =
            (+) a b |> (\x -> checkValue (String.fromInt a ++ " | " ++ String.fromInt b ++ " == " ++ String.fromInt x) x)

        updateMask : Char -> Int -> Mask -> Mask
        updateMask c bitValue mask =
            case readField c of
                One ->
                    { mask | onesMask = checkOr mask.onesMask bitValue }

                Zero ->
                    { mask | zeroesMask = checkOr mask.zeroesMask bitValue }

                _ ->
                    mask

        processField : Char -> ( Int, Mask ) -> ( Int, Mask )
        processField c state =
            case state of
                ( exponent, mask ) ->
                    ( exponent + 1, updateMask c (2 ^ exponent) mask )
    in
    String.toList maskString
        |> List.foldr processField ( 0, Mask 0 0 )
        |> Tuple.second
