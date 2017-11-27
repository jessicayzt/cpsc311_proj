module Game.Update exposing (..)

import Game.Avatar.Model as Avatar
import Game.Avatar.Update as Avatar
import Game.GamePlatform.Model as GamePlatform
import Game.GamePlatform.Update exposing (..)
import Game.Model exposing (..)
import List.Extra exposing (replaceIf)
import Time exposing (Time)
import ViewUtil exposing (..)
import Window exposing (Size)


type Msg
    = TimeUpdate Time
    | MsgForAvatar Avatar.Msg
    | Resize Size
    | NewPlatform GamePlatform.PlatformToGenerate
    | NoOp


type OutMsg
    = OutNoOp
    | GeneratePlatform


update : Msg -> Model -> Model
update msg game =
    let
        currentPlatform =
            List.head (List.filter ((\platform -> platform game.avatar) onGivenPlatform) game.platforms)
    in
    case msg of
        TimeUpdate msg_ ->
            if game.avatar.hp <= 0 || game.avatar.y < ViewUtil.pit then
                { game | state = Over }
            else if game.state == Playing then
                updateGame game currentPlatform
            else
                game

        MsgForAvatar msg_ ->
            case currentPlatform of
                Just platform ->
                    { game | avatar = Avatar.update msg_ game.avatar True }

                Nothing ->
                    { game | avatar = Avatar.update msg_ game.avatar False }

        NewPlatform platformToGenerate ->
            { game | platforms = extendPlatforms platformToGenerate game.platforms }

        Resize size ->
            { game | size = size }

        _ ->
            game


updateGame : Model -> Maybe GamePlatform.Model -> Model
updateGame game currentPlatform =
    let
        avatarColliding =
            isCollidingUnit game.avatar currentPlatform

        updatedPlatforms =
            if avatarColliding && hasCollectible currentPlatform then
                case currentPlatform of
                    Just currentPlatform ->
                        replaceIf (\platform -> platform == currentPlatform) (removeCollectible currentPlatform) game.platforms

                    Nothing ->
                        game.platforms
            else
                game.platforms
    in
    { game
        | platforms =
            if isSideScrolling game.avatar && game.avatar.vx /= 0 then
                List.map ((\platform -> platform game.avatar) scrollPlatform) (List.filter (\platform -> platform.x > -ViewUtil.width) updatedPlatforms)
            else
                updatedPlatforms
        , avatar = updatePhysics game.avatar avatarColliding currentPlatform
    }


updatePhysics : Avatar.Model -> Bool -> Maybe GamePlatform.Model -> Avatar.Model
updatePhysics avatar isColliding platform =
    case platform of
        Just platform ->
            if isColliding then
                physics (Avatar.updateStatus platform avatar) True
            else
                physics avatar True

        Nothing ->
            physics avatar False


physics : Avatar.Model -> Bool -> Avatar.Model
physics avatar standing =
    { avatar
        | x =
            if isSideScrolling avatar then
                0
            else
                avatar.x + avatar.vx
        , vx =
            if avatar.x <= -ViewUtil.halfWidth && avatar.dir == Avatar.Left then
                0
            else
                avatar.vx
        , y = avatar.y + avatar.vy
        , vy =
            if standing && avatar.vy <= 0 then
                0
            else
                avatar.vy - 1 / 2
        , score =
            if isSideScrolling avatar && avatar.vx /= 0 then
                avatar.score + 1
            else
                avatar.score
        , speed =
            Avatar.checkTimeLimit avatar
    }


isSideScrolling : Avatar.Model -> Bool
isSideScrolling avatar =
    avatar.x >= 0 && avatar.dir == Avatar.Right


extendPlatforms : GamePlatform.PlatformToGenerate -> List GamePlatform.Model -> List GamePlatform.Model
extendPlatforms newPlatform platforms =
    case List.head platforms of
        Just previousPlatform ->
            generatePlatform newPlatform previousPlatform :: platforms

        Nothing ->
            platforms


scrollPlatform : Avatar.Model -> GamePlatform.Model -> GamePlatform.Model
scrollPlatform avatar platform =
    { platform
        | x =
            platform.x + -avatar.vx
    }


onGivenPlatform : Avatar.Model -> GamePlatform.Model -> Bool
onGivenPlatform avatar platform =
    standingOn avatar platform
        && withinEdges avatar platform


standingOn : Avatar.Model -> GamePlatform.Model -> Bool
standingOn avatar platform =
    let
        platformStandingLevel =
            platform.y + platformBuffer
    in
    Basics.abs (platformStandingLevel - avatar.y) <= 15


withinEdges : Avatar.Model -> GamePlatform.Model -> Bool
withinEdges avatar platform =
    let
        rightEdge =
            platform.x + (platform.w / 2) + 5

        leftEdge =
            platform.x - (platform.w / 2) - 5
    in
    avatar.x
        <= rightEdge
        && avatar.x
        >= leftEdge


isCollidingUnit : Avatar.Model -> Maybe GamePlatform.Model -> Bool
isCollidingUnit avatar platform =
    case platform of
        Just platform ->
            Basics.abs (platform.x - avatar.x) <= 20

        Nothing ->
            False


updateOutMsg : Model -> OutMsg
updateOutMsg game =
    if List.length game.platforms <= 15 then
        GeneratePlatform
    else
        OutNoOp
