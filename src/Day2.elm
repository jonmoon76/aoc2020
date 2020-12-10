module Day2 exposing (..)

import Array exposing (Array(..))
import Parser exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set(..))
import Types exposing (..)


type alias Policy =
    { min : Int
    , max : Int
    , char : String
    }


type alias PolicyAndPassword =
    { policy : Policy
    , password : String
    }


isPasswordOK : Policy -> String -> Bool
isPasswordOK policy password =
    let
        checkCount : Int -> Bool
        checkCount n =
            if n >= policy.min && n <= policy.max then
                True

            else
                False

        getChar : Char
        getChar =
            String.toList policy.char
                |> List.head
                |> Maybe.withDefault ' '
    in
    String.toList password
        |> List.filter ((==) getChar)
        |> List.length
        |> checkCount


isPasswordOK2 : Policy -> String -> Bool
isPasswordOK2 policy password =
    let
        getChar : Char
        getChar =
            String.toList policy.char
                |> List.head
                |> Maybe.withDefault ' '

        check : Char -> Array Char -> Bool
        check c cs =
            case ( Array.get (policy.min - 1) cs, Array.get (policy.max - 1) cs ) of
                ( Just a, Just b ) ->
                    if (a == c || b == c) && (a /= b) then
                        Basics.True

                    else
                        Basics.False

                _ ->
                    False
    in
    String.toList password
        |> Array.fromList
        |> check getChar



-- 1-3 a


policyParser : Parser.Parser Policy
policyParser =
    Parser.succeed Policy
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int
        |. Parser.spaces
        |= Parser.variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }



-- 1-3 a: abcde


policyAndPasswordParser : Parser.Parser PolicyAndPassword
policyAndPasswordParser =
    Parser.succeed PolicyAndPassword
        |= policyParser
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.variable { start = Char.isLower, inner = Char.isLower, reserved = Set.empty }


parsePolicyAndPassword : String -> Maybe PolicyAndPassword
parsePolicyAndPassword input =
    Parser.run policyAndPasswordParser input
        |> Result.toMaybe


updateModel : Model -> WebData String -> Model
updateModel model input =
    { model | day2 = SingleFileModel input }


loadInput : Msg
loadInput =
    Run "day2_input.txt" updateModel


calculate : (Policy -> String -> Bool) -> String -> Int
calculate checker input =
    String.split "\n" input
        |> List.map parsePolicyAndPassword
        |> List.filterMap identity
        |> Debug.log "Start"
        |> List.filter (\x -> checker x.policy x.password)
        |> List.length
        |> Debug.log "End"


evaluate : (Policy -> String -> Bool) -> Day2Model -> Maybe Int
evaluate checker model =
    case model.input of
        Success input ->
            Just (calculate checker input)

        _ ->
            Nothing


part1 : Evaluator
part1 model =
    evaluate isPasswordOK model.day2


part2 : Evaluator
part2 model =
    evaluate isPasswordOK2 model.day2


init : Day1Model
init =
    { input = NotAsked }
