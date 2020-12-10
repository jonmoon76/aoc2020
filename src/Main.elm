module Main exposing (..)

import Browser
import Day1 exposing (..)
import Html exposing (Html, b, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)
import Types exposing (..)


loadInput : String -> ModelUpdater -> Cmd Msg
loadInput file updater =
    Http.get
        { url = file
        , expect =
            Http.expectString (RemoteData.fromResult >> InputLoaded updater)
        }


init : ( Model, Cmd Msg )
init =
    ( { day1 = Day1.init }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputLoaded updater inputData ->
            ( updater model inputData, Cmd.none )

        Run file updater ->
            ( model, loadInput file updater )


view : Model -> Html Msg
view model =
    div []
        [ Day1.view model
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
