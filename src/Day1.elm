module Day1 exposing (..)

import Browser
import Html exposing (Html, b, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
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


textify : Maybe Int -> Html Msg
textify result =
    Maybe.map String.fromInt result
        |> Maybe.withDefault "-"
        |> text


evaluate : Model -> Int -> Int -> Maybe Int
evaluate model target layers =
    case model.day1.input of
        Success input ->
            calculate 2020 2 input

        _ ->
            Nothing


init : Day1Model
init =
    { input = NotAsked }


view : Model -> Html Msg
view model =
    div [ class "exercise" ]
        [ text "Day 1"
        , text " "
        , button [ onClick loadInput ] [ text "Run" ]
        , text "Part 1:"
        , text " "
        , textify (evaluate model 2020 2)
        , text " "
        , text "Part 2:"
        , text " "
        , textify (evaluate model 2020 3)
        ]
