module Game.Avatar.Model exposing (..)

import ViewUtil exposing (..)


jumpVelocity : Float
jumpVelocity =
    10.0


defaultSpeed : Float
defaultSpeed =
    7.0


type Key
    = LeftArrowKey
    | RightArrowKey
    | SpaceBar
    | Escape
    | Enter
    | Unknown


type Direction
    = Left
    | Right


type alias Speed =
    { multiplier : Float
    , timeLimit : Float
    }

type alias Invincible =
    { isInvincible : Bool
    , timeLimit : Float
    }


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    , hp : Int
    , speed : Speed
    , invincible : Invincible
    , score : Int
    }


model : Model
model =
    { x = ViewUtil.left
    , y = ViewUtil.onGround
    , vx = 0
    , vy = 0
    , dir = Right
    , hp = 100
    , speed = { multiplier = 1.0, timeLimit = 0 }
    , invincible = { isInvincible = False, timeLimit = 0 }
    , score = 0
    }
