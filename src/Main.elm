module Main exposing (..)

import Browser
import Html exposing (Html, b, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { input : WebData String }


loadInput : Cmd Msg
loadInput =
    Http.get
        { url = "day1_input.txt"
        , expect =
            Http.expectString (RemoteData.fromResult >> InputLoaded)
        }


calculate_ : Int -> Int -> List Int -> List Int -> Maybe Int
calculate_ target layers candidates others =
    if layers == 0 then
        if List.sum candidates == target then
            Just (List.product candidates)

        else
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
        |> calculate_ target layers []


init : ( Model, Cmd Msg )
init =
    ( { input = NotAsked }, Cmd.none )


type Msg
    = InputLoaded (WebData String)
    | Run


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputLoaded inputData ->
            ( { model | input = inputData }, Cmd.none )

        Run ->
            ( { model | input = Loading }, loadInput )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Run ] [ text "Run" ]
        , text
            (Debug.toString
                (case model.input of
                    Success input ->
                        Just ( calculate 2020 2 input, calculate 2020 3 input )

                    _ ->
                        Nothing
                )
            )
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
