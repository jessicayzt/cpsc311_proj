module Game exposing (..)

import Avatar exposing (..)
import GamePlatform exposing (..)
import Key exposing (..)
import Keyboard exposing (KeyCode)
import Random exposing (..)
import Task exposing (..)
import Time exposing (Time, second)
import ViewUtil exposing (..)
import Window exposing (Size)


type State
    = Playing
    | Over


type alias Game =
    { size : Size
    , state : State
    , platforms : List GamePlatform
    , avatar : Avatar
    , score : Int
    }


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | NewPlatform PlatformToGenerate
    | Resize Size
    | NoOp


initGame : ( Game, Cmd Msg )
initGame =
    ( { size = Size 0 0
      , state = Playing
      , platforms = [ GamePlatform.init, GamePlatform.ground ]
      , avatar = initialAvatar
      , score = 0
      }
    , Task.perform Resize Window.size
    )


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        TimeUpdate newTime ->
            if game.state == Playing && List.length game.platforms < 15 then
                ( updateGame game
                , Random.generate NewPlatform platformGenerator
                )
            else
                ( updateGame game
                , Cmd.none
                )

        KeyDown keyCode ->
            if game.state == Playing then
                ( { game
                    | avatar = keyDown keyCode game.avatar game.platforms
                    , score = updateScore game keyCode
                  }
                , Cmd.none
                )
            else
                ( game, Cmd.none )

        KeyUp keyCode ->
            ( { game | avatar = keyUp keyCode game.avatar }, Cmd.none )

        NewPlatform platformToGenerate ->
            ( { game | platforms = extendPlatforms platformToGenerate game.platforms }, Cmd.none )

        Resize size ->
            ( { game | size = size }, Cmd.none )

        NoOp ->
            ( game, Cmd.none )


updateGame : Game -> Game
updateGame game =
    { game
        | state =
            if game.avatar.hp <= 0 then
                Over
            else
                Playing
        , platforms =
            if isSideScrolling game.avatar && game.avatar.vx /= 0 then
                List.map ((\platform -> platform game.avatar) scrollPlatform) (List.filter (\platform -> platform.x > -ViewUtil.width) game.platforms)
            else
                game.platforms
        , avatar = updateAvatar game game.platforms
    }


scrollPlatform : Avatar -> GamePlatform -> GamePlatform
scrollPlatform avatar platform =
    { platform
        | x =
            platform.x + -avatar.vx
    }


updateAvatar : Game -> List GamePlatform -> Avatar
updateAvatar game platforms =
    game.avatar
        |> constrainLeftEdge
        |> gravity platforms
        |> physics
        |> status platforms


status : List GamePlatform -> Avatar -> Avatar
status platforms avatar =
    { avatar
        | hp = updateHp platforms avatar
    }


updateHp : List GamePlatform -> Avatar -> Int
updateHp platforms avatar =
    let
        currentPlatform =
            List.head (List.filter ((\platform -> platform avatar) onGivenPlatform) platforms)
    in
    case currentPlatform of
        Just platform ->
            if isCollidingUnit avatar platform then
                case platform.unit of
                    Spikes ->
                        max (avatar.hp - 1) 0

                    Waste ->
                        max (avatar.hp - 10) 0

                    None ->
                        avatar.hp
            else
                avatar.hp

        Nothing ->
            if avatar.y < ViewUtil.pit then
                0
            else
                avatar.hp


isCollidingUnit : Avatar -> GamePlatform -> Bool
isCollidingUnit avatar platform =
    Basics.abs (platform.x - avatar.x) <= 20


updateScore : Game -> KeyCode -> Int
updateScore game keyCode =
    case Key.fromCode keyCode of
        RightArrowKey ->
            game.score + 1

        _ ->
            game.score


keyDown : KeyCode -> Avatar -> List GamePlatform -> Avatar
keyDown keyCode avatar platforms =
    case Key.fromCode keyCode of
        LeftArrowKey ->
            walk -Avatar.defaultSpeed avatar

        RightArrowKey ->
            walk Avatar.defaultSpeed avatar

        SpaceBar ->
            jump avatar platforms

        _ ->
            avatar


keyUp : KeyCode -> Avatar -> Avatar
keyUp keyCode avatar =
    case Key.fromCode keyCode of
        LeftArrowKey ->
            walk 0 avatar

        RightArrowKey ->
            walk 0 avatar

        _ ->
            avatar
