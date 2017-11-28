module Game.GamePlatform.Model exposing (..)

import Random exposing (Generator)
import Time exposing (Time, second)
import ViewUtil exposing (..)


type alias Model =
    { w : Float
    , x : Float
    , y : Float
    , unit : Unit
    }


type alias PlatformToGenerate =
    { w : Float
    , heightDiff : Float
    , unit : Unit
    }


type Unit
    = Spikes
    | Waste
    | Zombie ( Float, Direction )
    | HP
    | TwoBones
    | ThreeBones
    | Boost
    | Shield
    | None


type Direction
    = Left
    | Right


model : Model
model =
    Model 200 (-ViewUtil.halfWidth / 2) (-ViewUtil.halfHeight / 2) Spikes


ground : Model
ground =
    Model (width / 2) -ViewUtil.halfWidth (-ViewUtil.halfHeight + (ViewUtil.platformHeight / 2)) Spikes
