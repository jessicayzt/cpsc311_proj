module Game exposing (..)

import Avatar exposing (..)
import GamePlatform exposing (..)
import Key exposing (..)
import Keyboard exposing (KeyCode)
import List.Extra exposing (replaceIf)
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
            if game.state == Playing then
                if List.length game.platforms <= 15 then
                    ( updateGame game
                    , Random.generate NewPlatform platformGenerator
                    )
                else
                    ( updateGame game
                    , Cmd.none
                    )
            else
                ( game
                , Cmd.none
                )

        KeyDown keyCode ->
            if game.state == Playing then
                ( { game
                    | avatar = keyDown keyCode game.avatar game.platforms
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
    let
        currentPlatform =
            List.head (List.filter ((\platform -> platform game.avatar) onGivenPlatform) game.platforms)

        updatedPlatforms =
            if isCollidingUnit game.avatar currentPlatform && hasCollectible currentPlatform then
                case currentPlatform of
                    Just currentPlatform ->
                        replaceIf (\platform -> platform == currentPlatform) (removeCollectible currentPlatform) game.platforms

                    Nothing ->
                        game.platforms
            else
                game.platforms
    in
    { game
        | state =
            if game.avatar.hp <= 0 then
                Over
            else
                Playing
        , platforms =
            if isSideScrolling game.avatar && game.avatar.vx /= 0 then
                List.map ((\platform -> platform game.avatar) scrollPlatform) (List.filter (\platform -> platform.x > -ViewUtil.width) updatedPlatforms)
            else
                updatedPlatforms
        , avatar = updateAvatar game currentPlatform
        , score = updateScore game currentPlatform
    }


scrollPlatform : Avatar -> GamePlatform -> GamePlatform
scrollPlatform avatar platform =
    { platform
        | x =
            platform.x + -avatar.vx
    }


updateAvatar : Game -> Maybe GamePlatform -> Avatar
updateAvatar game platform =
    game.avatar
        |> constrainLeftEdge
        |> gravity platform
        |> physics
        |> status platform


status : Maybe GamePlatform -> Avatar -> Avatar
status platform avatar =
    { avatar
        | hp = updateHp platform avatar
        , speed = updateSpeed platform avatar
    }


updateHp : Maybe GamePlatform -> Avatar -> Int
updateHp platform avatar =
    if isCollidingUnit avatar platform then
        case platform of
            Just platform ->
                case platform.unit of
                    Spikes ->
                        max (avatar.hp - 1) 0

                    Waste ->
                        max (avatar.hp - 10) 0

                    HP ->
                        min (avatar.hp + 10) 100

                    _ ->
                        avatar.hp

            Nothing ->
                avatar.hp
    else if avatar.y < ViewUtil.pit then
        0
    else
        avatar.hp


updateSpeed : Maybe GamePlatform -> Avatar -> Speed
updateSpeed platform avatar =
    if isCollidingUnit avatar platform then
        case platform of
            Just platform ->
                case platform.unit of
                    Boost ->
                        { multiplier = min (avatar.speed.multiplier + 0.5) 2.0
                        , timeLimit = 100
                        }

                    _ ->
                        avatar.speed

            Nothing ->
                avatar.speed
    else if avatar.y < ViewUtil.pit then
        avatar.speed
    else
        checkTimeLimit avatar


checkTimeLimit : Avatar -> Speed
checkTimeLimit avatar =
    if avatar.speed.timeLimit > 0 then
        { multiplier = avatar.speed.multiplier
        , timeLimit = max (avatar.speed.timeLimit - 1) 0
        }
    else
        { multiplier = 1.0
        , timeLimit = 0
        }


isCollidingUnit : Avatar -> Maybe GamePlatform -> Bool
isCollidingUnit avatar platform =
    case platform of
        Just platform ->
            Basics.abs (platform.x - avatar.x) <= 20

        Nothing ->
            False


updateScore : Game -> Maybe GamePlatform -> Int
updateScore game platform =
    if isCollidingUnit game.avatar platform then
        case platform of
            Just platform ->
                case platform.unit of
                    TwoBones ->
                        game.score + 50

                    ThreeBones ->
                        game.score + 100

                    _ ->
                        game.score

            Nothing ->
                game.score
    else if isSideScrolling game.avatar && game.avatar.vx /= 0 then
        game.score + 1
    else
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
