module Main exposing (..)

import Browser
import Day1
import Day10
import Day11
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
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
      , day6 = Day6.init
      , day7 = Day7.init
      , day8 = Day8.init
      , day9 = Day9.init
      , day10 = Day10.init
      , day11 = Day11.init
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


viewDay2 : DayModel -> String -> Html Msg
viewDay2 dayModel title =
    let
        record : DayModelRecord
        record =
            case dayModel of
                DayModel d ->
                    d

        storeInput : Model -> WebData String -> Model
        storeInput model webDataInput =
            record.updateDayModel model (DayModel { record | input = webDataInput })

        resultSet : String -> Maybe String -> Html Msg
        resultSet subtitle input =
            div []
                [ text subtitle
                , text " "
                , text "Part 1:"
                , text " "
                , textify <| Maybe.andThen record.part1 input
                , text " "
                , text "Part 2:"
                , text " "
                , textify <| Maybe.andThen record.part2 input
                ]
    in
    div [ class "exercise" ]
        [ button [ onClick (Run record.inputFile storeInput) ] [ text title ]
        , resultSet "Main" (RemoteData.toMaybe record.input)
        , resultSet "Example" record.example
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewDay2 model.day11 "Day 11"
        , viewDay2 model.day10 "Day 10"
        , viewDay2 model.day9 "Day 9"
        , viewDay2 model.day8 "Day 8"
        , viewDay2 model.day7 "Day 7"
        , viewDay2 model.day6 "Day 6"
        , viewDay2 model.day5 "Day 5"
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
