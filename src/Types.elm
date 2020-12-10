module Types exposing (..)

import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { day1 : Day1Model }


type alias Day1Model =
    { input : WebData String }


type alias ModelUpdater =
    Model -> WebData String -> Model


type Msg
    = InputLoaded ModelUpdater (WebData String)
    | Run String ModelUpdater
