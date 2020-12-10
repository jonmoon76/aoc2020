module Day3 exposing (..)

import Array exposing (Array(..))
import Parser exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set(..))
import Types exposing (..)


updateModel : Model -> WebData String -> Model
updateModel model input =
    { model | day3 = SingleFileModel input }


loadInput : Msg
loadInput =
    Run "day3_input.txt" updateModel


init : SingleFileModel
init =
    { input = NotAsked }


calculate : String -> Maybe Int
calculate input =
    Nothing


evaluate : SingleFileModel -> Maybe Int
evaluate model =
    case model.input of
        Success input ->
            calculate input

        _ ->
            Nothing


part1 : Evaluator
part1 model =
    evaluate model.day3


part2 : Evaluator
part2 model =
    evaluate model.day3
