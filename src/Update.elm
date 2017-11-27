module Update exposing (..)

import Game.Avatar.Model as Avatar
import Game.Avatar.Update as Avatar
import Game.GamePlatform.Model as GamePlatform
import Game.GamePlatform.Update exposing (..)
import Game.Model as Game
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
    | InputName String
    | NewPlatform GamePlatform.PlatformToGenerate
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        TimeUpdate time ->
            { model | game = Game.update (Game.TimeUpdate time) model.game }

        KeyDown keyCode ->
            if model.screen == StartScreen && Avatar.Enter == fromCode keyCode then
                { model | screen = PlayScreen }
            else if model.screen == GameOverScreen && Avatar.Enter == fromCode keyCode then
                { model
                    | game = Game.model
                    , screen = StartScreen
                    , highScores = List.sortWith sortScores (appendScore model.highScores { name = model.playerName, score = model.game.avatar.score })
                }
            else if Avatar.Escape == fromCode keyCode then
                { model
                    | game = Game.model
                    , screen = StartScreen
                }
            else if model.screen == PlayScreen then
                { model | game = Game.update (Game.MsgForAvatar (Avatar.KeyDown (fromCode keyCode))) model.game }
            else
                model

        KeyUp keyCode ->
            { model | game = Game.update (Game.MsgForAvatar (Avatar.KeyUp (fromCode keyCode))) model.game }

        Resize size ->
            { model | size = size }

        NewPlatform generatedPlatform ->
            { model | game = Game.update (Game.NewPlatform generatedPlatform) model.game }

        InputName name ->
            { model
                | playerName = name
            }

        _ ->
            model
    )
        |> updateOutMsg msg


updateOutMsg : Msg -> Model -> ( Model, Cmd Msg )
updateOutMsg msg model =
    case Game.updateOutMsg model.game of
        Game.GeneratePlatform ->
            ( model, Random.generate NewPlatform platformGenerator )

        Game.NewScore score ->
            ( { model | screen = GameOverScreen }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


sortScores : HighScore -> HighScore -> Order
sortScores hs1 hs2 =
    if hs1.score < hs2.score then
        GT
    else if hs1.score > hs2.score then
        LT
    else
        EQ


appendScore : List HighScore -> HighScore -> List HighScore
appendScore list hs =
    if List.length list < 3 then
        List.append list [ hs ]
    else
        case List.head (List.drop 2 list) of
            Just listTail ->
                if listTail.score <= hs.score then
                    List.append (List.take 2 list) [ hs ]
                else
                    list

            Nothing ->
                list


fromCode : Int -> Avatar.Key
fromCode keyCode =
    case keyCode of
        13 ->
            Avatar.Enter

        27 ->
            Avatar.Escape

        37 ->
            Avatar.LeftArrowKey

        39 ->
            Avatar.RightArrowKey

        32 ->
            Avatar.SpaceBar

        _ ->
            Avatar.Unknown
