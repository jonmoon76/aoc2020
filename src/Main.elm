module Main exposing (..)

import Browser
import Day1
import Day2
import Day3
import Day4
import Day5
import Html exposing (Html, b, button, div, text)
import Html.Attributes exposing (class)
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
    ( { day1 = Day1.init
      , day2 = Day2.init
      , day3 = Day3.init
      , day4 = Day4.init
      , day5 = Day5.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputLoaded updater inputData ->
            ( updater model inputData, Cmd.none )

        Run file updater ->
            ( model, loadInput file updater )


textify : Maybe Int -> Html Msg
textify result =
    Maybe.map String.fromInt result
        |> Maybe.withDefault "-"
        |> text


viewDay : Model -> String -> Msg -> Evaluator -> Evaluator -> Html Msg
viewDay model title loader part1 part2 =
    div [ class "exercise" ]
        [ button [ onClick loader ] [ text title ]
        , text "Part 1:"
        , text " "
        , textify <| part1 model
        , text " "
        , text "Part 2:"
        , text " "
        , textify <| part2 model
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewDay model "Day 5" Day5.loadInput Day5.part1 Day5.part2
        , viewDay model "Day 4" Day4.loadInput Day4.part1 Day4.part2
        , viewDay model "Day 3" Day3.loadInput Day3.part1 Day3.part2
        , viewDay model "Day 2" Day2.loadInput Day2.part1 Day2.part2
        , viewDay model "Day 1" Day1.loadInput Day1.part1 Day1.part2
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
