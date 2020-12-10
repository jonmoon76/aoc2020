module Day1 exposing (..)

import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set(..))
import Types exposing (..)


updateModel : Model -> WebData String -> Model
updateModel model input =
    { model | day1 = SingleFileModel input }


loadInput : Msg
loadInput =
    Run "day1_input.txt" updateModel


calculate_ : Int -> Int -> List Int -> List Int -> Maybe Int
calculate_ target layers candidates others =
    if layers == 0 then
        if List.sum candidates == target then
            Just (List.product candidates)

        else
            Nothing

    else if List.sum candidates >= target then
        Nothing

    else
        case others of
            [] ->
                Nothing

            x :: xs ->
                case calculate_ target (layers - 1) (x :: candidates) xs of
                    Just r ->
                        Just r

                    Nothing ->
                        calculate_ target layers candidates xs


calculate : Int -> Int -> String -> Maybe Int
calculate target layers input =
    String.split "\n" input
        |> List.map String.toInt
        |> List.filterMap identity
        |> Debug.log "Start"
        |> List.sort
        |> List.reverse
        |> calculate_ target layers []
        |> Debug.log "End"


evaluate : Model -> Int -> Int -> Maybe Int
evaluate model target layers =
    case model.day1.input of
        Success input ->
            calculate target layers input

        _ ->
            Nothing


part1 : Model -> Maybe Int
part1 model =
    evaluate model 2020 2


part2 : Model -> Maybe Int
part2 model =
    evaluate model 2020 3


init : Day1Model
init =
    { input = NotAsked }
