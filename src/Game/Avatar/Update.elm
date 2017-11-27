module Game.Avatar.Update exposing (..)

import Game.Avatar.Model exposing (..)
import Game.GamePlatform.Model as GamePlatform
import ViewUtil exposing (..)


type Msg
    = KeyDown Key
    | KeyUp Key


update : Msg -> Model -> Bool -> Model
update msg avatar isStanding =
    case msg of
        KeyDown key ->
            keyDown key avatar isStanding

        KeyUp key ->
            { avatar
                | vx =
                    if key == SpaceBar then
                        avatar.vx
                    else
                        0
            }


keyDown : Key -> Model -> Bool -> Model
keyDown key avatar isStanding =
    case key of
        LeftArrowKey ->
            walk -defaultSpeed avatar

        RightArrowKey ->
            walk defaultSpeed avatar

        SpaceBar ->
            jump avatar isStanding

        _ ->
            avatar


jump : Model -> Bool -> Model
jump avatar isStanding =
    if isStanding then
        { avatar | vy = jumpVelocity }
    else
        avatar


walk : Float -> Model -> Model
walk newVx avatar =
    { avatar
        | dir =
            if newVx < 0 then
                Left
            else if newVx > 0 then
                Right
            else
                avatar.dir
        , vx =
            if avatar.x <= -ViewUtil.halfWidth && avatar.dir == Left then
                0
            else
                newVx * avatar.speed.multiplier
    }


updateStatus : GamePlatform.Model -> Model -> Model
updateStatus platform avatar =
    { avatar
        | hp = updateHp platform avatar
        , speed = updateSpeed platform avatar
        , score = updateScore platform avatar
    }


updateHp : GamePlatform.Model -> Model -> Int
updateHp platform avatar =
    case platform.unit of
        GamePlatform.Spikes ->
            max (avatar.hp - 1) 0

        GamePlatform.Waste ->
            max (avatar.hp - 10) 0

        GamePlatform.HP ->
            min (avatar.hp + 10) 100

        _ ->
            avatar.hp


updateSpeed : GamePlatform.Model -> Model -> Speed
updateSpeed platform avatar =
    case platform.unit of
        GamePlatform.Boost ->
            { multiplier = min (avatar.speed.multiplier + 0.5) 2.0
            , timeLimit = 100
            }

        _ ->
            checkTimeLimit avatar


checkTimeLimit : Model -> Speed
checkTimeLimit avatar =
    if avatar.speed.timeLimit > 0 then
        { multiplier = avatar.speed.multiplier
        , timeLimit = max (avatar.speed.timeLimit - 1) 0
        }
    else
        { multiplier = 1.0
        , timeLimit = 0
        }


updateScore : GamePlatform.Model -> Model -> Int
updateScore platform avatar =
    case platform.unit of
        GamePlatform.TwoBones ->
            avatar.score + 50

        GamePlatform.ThreeBones ->
            avatar.score + 100

        _ ->
            avatar.score
