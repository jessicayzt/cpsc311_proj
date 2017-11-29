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
                        replaceIf (\platform -> platform == currentPlatform) (removeCollectible currentPlatform) (updatePlatformUnits game.platforms)

                    Nothing ->
                        updatePlatformUnits game.platforms
            else
                updatePlatformUnits game.platforms

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


updatePlatformUnits : List GamePlatform.Model -> List GamePlatform.Model
updatePlatformUnits platforms =
    List.map movePlatformUnit platforms


movePlatformUnit : GamePlatform.Model -> GamePlatform.Model
movePlatformUnit platform =
    case platform.unit of
        GamePlatform.Zombie status ->
            let
                currentDir =
                    Tuple.second status
            in
            { platform
                | unit =
                    case currentDir of
                        GamePlatform.Left ->
                            let
                                newOffset =
                                    Tuple.first status - 1

                                newDir =
                                    if unitAtLeftEdge platform newOffset then
                                        GamePlatform.Right
                                    else
                                        GamePlatform.Left
                            in
                            GamePlatform.Zombie ( newOffset, newDir )

                        _ ->
                            let
                                newOffset =
                                    Tuple.first status + 1

                                newDir =
                                    if unitAtRightEdge platform newOffset then
                                        GamePlatform.Left
                                    else
                                        GamePlatform.Right
                            in
                            GamePlatform.Zombie ( newOffset, newDir )
            }

        _ ->
            platform


unitAtLeftEdge : GamePlatform.Model -> Float -> Bool
unitAtLeftEdge platform offset =
    let
        leftEdge =
            platform.x - (platform.w / 2) + 15
    in
    platform.x
        + offset
        <= leftEdge


unitAtRightEdge : GamePlatform.Model -> Float -> Bool
unitAtRightEdge platform offset =
    let
        rightEdge =
            platform.x + (platform.w / 2) - 15
    in
    platform.x
        + offset
        >= rightEdge


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
            case platform.unit of
                GamePlatform.Zombie status ->
                    Basics.abs ((platform.x + Tuple.first status) - avatar.x) <= 20

                _ ->
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
