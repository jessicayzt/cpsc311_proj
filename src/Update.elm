module Update exposing (..)

import Game.Avatar.Model as Avatar
import Game.Avatar.Update as Avatar
import Game.GamePlatform.Model as GamePlatform
import Game.GamePlatform.Update exposing (..)
import Game.Update as Game
import Keyboard exposing (KeyCode)
import Model exposing (..)
import Random exposing (..)
import Time exposing (Time)
import Window exposing (Size)


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Resize Size
    | NewPlatform GamePlatform.PlatformToGenerate
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        TimeUpdate msg_ ->
            { model | game = Game.update (Game.TimeUpdate msg_) model.game }

        KeyDown msg_ ->
            { model | game = Game.update (Game.MsgForAvatar (Avatar.KeyDown (fromCode msg_))) model.game }

        KeyUp msg_ ->
            { model | game = Game.update (Game.MsgForAvatar (Avatar.KeyUp (fromCode msg_))) model.game }

        Resize msg_ ->
            { model | game = Game.update (Game.Resize msg_) model.game }

        NewPlatform msg_ ->
            { model | game = Game.update (Game.NewPlatform msg_) model.game }

        _ ->
            model
    )
        |> updateOutMsg msg


updateOutMsg : Msg -> Model -> ( Model, Cmd Msg )
updateOutMsg msg model =
    case Game.updateOutMsg model.game of
        Game.GeneratePlatform ->
            ( model, Random.generate NewPlatform platformGenerator )

        _ ->
            ( model, Cmd.none )


fromCode : Int -> Avatar.Key
fromCode keyCode =
    case keyCode of
        37 ->
            Avatar.LeftArrowKey

        39 ->
            Avatar.RightArrowKey

        32 ->
            Avatar.SpaceBar

        _ ->
            Avatar.Unknown
