module Avatar exposing (..)

import GamePlatform exposing (..)
import ViewUtil exposing (..)


jumpVelocity : Float
jumpVelocity =
    8.0


defaultSpeed : Float
defaultSpeed =
    6.0

type alias Speed =
  { multiplier : Float
  , timeLimit : Int
  }

type alias Avatar =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    , hp : Int
    , speed : Speed
    , invincible : Bool
    }


type Direction
    = Left
    | Right


initialAvatar : Avatar
initialAvatar =
    { x = ViewUtil.left
    , y = ViewUtil.onGround
    , vx = 0
    , vy = 0
    , dir = Right
    , hp = 100
    , speed = { multiplier = 1.0 , timeLimit = 0 }
    , invincible = False
    }

jump : Avatar -> List GamePlatform -> Avatar
jump avatar platforms =
    if avatar.vy == 0 && onPlatform avatar platforms then
        { avatar | vy = jumpVelocity }
    else
        avatar


walk : Float -> Avatar -> Avatar
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
            newVx * avatar.speed.multiplier
    }


constrainLeftEdge : Avatar -> Avatar
constrainLeftEdge avatar =
    { avatar
        | vx =
            if avatar.x == -ViewUtil.halfWidth && avatar.dir == Left then
                0
            else
                avatar.vx
    }


gravity : Maybe GamePlatform -> Avatar -> Avatar
gravity platform avatar =
  case platform of
    Just platform ->
      { avatar
          | vy =
              if avatar.vy <= 0 then
                  0
              else
                  avatar.vy - 1 / 4
      }
    Nothing ->
      { avatar
        | vy = avatar.vy - 1 / 4
      }


onPlatform : Avatar -> List GamePlatform -> Bool
onPlatform avatar platforms =
    List.any ((\platform -> platform avatar) onGivenPlatform) platforms


onGivenPlatform : Avatar -> GamePlatform -> Bool
onGivenPlatform avatar platform =
    standingOn avatar platform
        && withinEdges avatar platform


standingOn : Avatar -> GamePlatform -> Bool
standingOn avatar platform =
    let
        platformStandingLevel =
            platform.y + platformBuffer
    in
    Basics.abs (platformStandingLevel - avatar.y) <= 15


withinEdges : Avatar -> GamePlatform -> Bool
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


physics : Avatar -> Avatar
physics avatar =
    { avatar
        | x =
            if isSideScrolling avatar then
                0
            else
                avatar.x + avatar.vx
        , y = avatar.y + avatar.vy
    }


isSideScrolling : Avatar -> Bool
isSideScrolling avatar =
    avatar.x >= 0 && avatar.dir == Right
