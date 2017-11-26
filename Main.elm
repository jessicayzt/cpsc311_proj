module Main exposing (..)

import AnimationFrame exposing (..)
import Html exposing (..)
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Task exposing (..)
import Update exposing (..)
import View exposing (..)
import Window exposing (Size)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions subscriptions =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes Resize
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Task.perform Resize Window.size )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
