module Game.Update exposing (..)

import Game.Avatar.Model as Avatar
import Game.Avatar.Update as Avatar
import Game.GamePlatform.Model as GamePlatform
import Game.GamePlatform.Update exposing (..)
import Game.Model exposing (..)
import List.Extra exposing (replaceIf)
import Time exposing (Time)
import ViewUtil exposing (..)


type Msg
    = TimeUpdate Time
    | MsgForAvatar Avatar.Msg
    | NewPlatform GamePlatform.PlatformToGenerate


type OutMsg
    = OutNoOp
    | GeneratePlatform
    | NewScore Int


update : Msg -> Model -> Model
update msg game =
    case msg of
        TimeUpdate elapsed ->
            if game.avatar.hp <= 0 || game.avatar.y < ViewUtil.pit then
                { game | state = Over }
            else if game.state == Playing then
                updateGame game elapsed
            else
                game

        MsgForAvatar msg_ ->
            { game | avatar = Avatar.update msg_ game.avatar (List.any ((\platform -> platform game.avatar) onGivenPlatform) game.platforms) }

        NewPlatform platformToGenerate ->
            { game | platforms = extendPlatforms platformToGenerate game.platforms }


updateGame : Model -> Time -> Model
updateGame game lag =
    let
        motionMultipler =
            if lag >= msPerUpdate then
                1
            else
                lag / msPerUpdate

        newGame =
            updateMotion game motionMultipler
    in
    if lag >= msPerUpdate then
        updateGame newGame (lag - msPerUpdate)
    else
        newGame


updateMotion : Model -> Float -> Model
updateMotion game motionMultiplier =
    let
        currentPlatform =
            List.head (List.filter ((\platform -> platform game.avatar) onGivenPlatform) game.platforms)

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

        movedAvatar =
            case currentPlatform of
                Just currentPlatform ->
                    if avatarColliding then
                        physics (Avatar.updateStatus currentPlatform game.avatar) motionMultiplier True
                    else
                        physics game.avatar motionMultiplier True

                Nothing ->
                    physics game.avatar motionMultiplier False
    in
    { game
        | platforms =
            if isSideScrolling game.avatar && game.avatar.vx /= 0 then
                List.map ((\platform -> platform game.avatar motionMultiplier) scrollPlatform) (List.filter (\platform -> platform.x > -ViewUtil.width) updatedPlatforms)
            else
                updatedPlatforms
        , avatar = movedAvatar
    }


physics : Avatar.Model -> Float -> Bool -> Avatar.Model
physics avatar multiplier standing =
    { avatar
        | x =
            if isSideScrolling avatar then
                0
            else
                avatar.x + (avatar.vx * multiplier)
        , vx =
            if avatar.x <= -ViewUtil.halfWidth && avatar.dir == Avatar.Left then
                0
            else
                avatar.vx
        , y = avatar.y + (avatar.vy * multiplier)
        , vy =
            if standing && avatar.vy <= 0 then
                0
            else
                avatar.vy - (multiplier * 0.4)
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


scrollPlatform : Avatar.Model -> Float -> GamePlatform.Model -> GamePlatform.Model
scrollPlatform avatar motionMultiplier platform =
    { platform
        | x =
            platform.x - (avatar.vx * motionMultiplier)
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
    Basics.abs (platformStandingLevel - avatar.y) <= 20


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
    if game.state == Over then
        NewScore game.avatar.score
    else if List.length game.platforms <= 15 then
        GeneratePlatform
    else
        OutNoOp
