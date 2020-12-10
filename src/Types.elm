module Types exposing (..)

import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { day1 : Day1Model
    , day2 : Day2Model
    }


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
