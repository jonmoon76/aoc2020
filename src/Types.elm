module Types exposing (..)

import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { day1 : Day1Model
    , day2 : Day2Model
    , day3 : SingleFileModel
    , day4 : SingleFileModel
    , day5 : SingleFileModel
    , day6 : DayModel
    }


type alias DayModelRecord =
    { example : Maybe String
    , updateDayModel : Model -> DayModel -> Model
    , input : WebData String
    , inputFile : String
    , part1 : Maybe String -> Maybe Int
    , part2 : Maybe String -> Maybe Int
    }


type DayModel
    = DayModel DayModelRecord


type alias SingleFileModel =
    { input : WebData String }


type alias Day1Model =
    SingleFileModel


type alias Day2Model =
    SingleFileModel


type alias ModelUpdater =
    Model -> WebData String -> Model


type alias Evaluator =
    Model -> Maybe Int


type Msg
    = InputLoaded ModelUpdater (WebData String)
    | Run String ModelUpdater
