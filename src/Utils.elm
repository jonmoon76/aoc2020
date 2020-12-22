module Utils exposing (..)


trace : String -> a -> (() -> x) -> x
trace label args fx =
    let
        label_ =
            label ++ " " ++ Debug.toString args

        _ =
            Debug.log label_ "ENTER"
    in
    let
        x =
            fx ()

        _ =
            Debug.log label_ x
    in
    x
