module Key exposing (..)


type Key
    = LeftArrowKey
    | RightArrowKey
    | SpaceBar
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            LeftArrowKey

        39 ->
            RightArrowKey

        32 ->
            SpaceBar

        _ ->
            Unknown
