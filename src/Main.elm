module Main exposing (..)

import AnimationFrame exposing (..)
import Game exposing (..)
import Html exposing (..)
import Keyboard exposing (KeyCode)
import View exposing (..)
import Window exposing (Size)


-- SUBSCRIPTIONS


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes Resize
        ]



-- MAIN


main : Program Never Game Msg
main =
    Html.program
        { init = initGame
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
