module Day8 exposing (..)

import Array exposing (Array(..))
import Bootstrap.Form exposing (Col)
import Dict exposing (..)
import Html exposing (input)
import Maybe exposing (Maybe(..))
import Parser exposing ((|.), (|=), run)
import RemoteData exposing (RemoteData(..), WebData)
import Result
import Set exposing (Set(..))
import Tuple exposing (..)
import Types exposing (..)


init : DayModel
init =
    DayModel
        { example = Just example
        , updateDayModel = \model daymodel -> { model | day8 = daymodel }
        , input = NotAsked
        , inputFile = "day8_input.txt"
        , part1 = part1
        , part2 = part2
        }


example : String
example =
    """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""


part1 : String -> Maybe Int
part1 input =
    Just <|
        (loadProgram input
            |> runProgramGetAcc
        )


part2 : String -> Maybe Int
part2 input =
    loadProgram input
        |> fixProgram
        |> Maybe.map runProgramGetAcc


type OpCode
    = Nop
    | Acc
    | Jmp


type alias Op =
    { opCode : OpCode
    , value : Int
    }


type alias Program =
    Array Op


type ExecutionResult
    = Terminated
    | Looped
    | InvalidPC


type alias State =
    { pc : Int
    , acc : Int
    , pcHistory : List Int
    }


type alias ProgramResult =
    { executionResult : ExecutionResult
    , finalState : State
    }


loadProgram : String -> Program
loadProgram input =
    let
        opCodeParser : Parser.Parser OpCode
        opCodeParser =
            Parser.oneOf
                [ Parser.symbol "nop" |> Parser.map (\x -> Nop)
                , Parser.symbol "jmp" |> Parser.map (\x -> Jmp)
                , Parser.symbol "acc" |> Parser.map (\x -> Acc)
                ]

        signedIntParser : Parser.Parser Basics.Int
        signedIntParser =
            Parser.oneOf
                [ Parser.succeed negate
                    |. Parser.symbol "-"
                    |= Parser.int
                , Parser.succeed identity
                    |. Parser.symbol "+"
                    |= Parser.int
                ]

        opParser : Parser.Parser Op
        opParser =
            Parser.succeed Op
                |= opCodeParser
                |. Parser.symbol " "
                |= signedIntParser
                |. Parser.end

        parseOp : String -> Maybe Op
        parseOp line =
            line |> Parser.run opParser |> Result.toMaybe
    in
    String.split "\n" input
        |> List.filterMap parseOp
        |> Array.fromList


initialState : State
initialState =
    { pc = 0
    , acc = 0
    , pcHistory = []
    }


runProgramGetAcc : Program -> Int
runProgramGetAcc program =
    runProgram program
        |> (.finalState >> .acc)


runProgram : Program -> ProgramResult
runProgram program =
    runProgramHelp initialState program


runProgramHelp : State -> Program -> ProgramResult
runProgramHelp state program =
    let
        executeOpHelp : Int -> Int -> ProgramResult
        executeOpHelp pc acc =
            runProgramHelp { state | pc = pc, acc = acc, pcHistory = state.pc :: state.pcHistory } program

        executeOp : Op -> ProgramResult
        executeOp op =
            case op.opCode of
                Nop ->
                    executeOpHelp (state.pc + 1) state.acc

                Jmp ->
                    executeOpHelp (state.pc + op.value) state.acc

                Acc ->
                    executeOpHelp (state.pc + 1) (state.acc + op.value)
    in
    if Set.member state.pc (Set.fromList state.pcHistory) then
        ProgramResult Looped state

    else
        case Array.get state.pc program of
            Nothing ->
                ProgramResult
                    (if state.pc == Array.length program then
                        Terminated

                     else
                        InvalidPC
                    )
                    state

            Just op ->
                executeOp op


fixProgram : Program -> Maybe Program
fixProgram program =
    let
        isCandidate : Op -> Bool
        isCandidate op =
            op.opCode /= Acc

        checkCandidate : Int -> Maybe Int
        checkCandidate pc =
            Array.get pc program
                |> Maybe.andThen
                    (\op ->
                        if isCandidate op then
                            Just pc

                        else
                            Nothing
                    )

        identifyCandidates : List Int
        identifyCandidates =
            runProgram program
                |> (.finalState >> .pcHistory)
                |> List.filterMap checkCandidate
                |> List.reverse

        flipOp : Op -> Op
        flipOp op =
            case op.opCode of
                Jmp ->
                    Op Nop op.value

                Nop ->
                    Op Nop op.value

                _ ->
                    op

        modifyProgram : Int -> Maybe Program
        modifyProgram pc =
            Array.get pc program
                |> Maybe.map (\x -> Array.set pc (flipOp x) program)

        testCandidate : Int -> Bool
        testCandidate pc =
            Maybe.withDefault False <|
                (modifyProgram pc
                    |> Maybe.map runProgram
                    |> Maybe.map
                        (\x -> x.executionResult == Terminated)
                )

        tryCandidates : List Int -> Maybe Program
        tryCandidates candidates =
            case candidates of
                [] ->
                    Nothing

                c :: cs ->
                    if testCandidate c then
                        modifyProgram c

                    else
                        tryCandidates cs
    in
    identifyCandidates
        |> tryCandidates
