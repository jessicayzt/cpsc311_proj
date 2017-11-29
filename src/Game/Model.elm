module Game.Model exposing (..)

import Game.Avatar.Model as Avatar
import Game.GamePlatform.Model as GamePlatform
import Task exposing (..)
import Window exposing (Size)


msPerUpdate : Float
msPerUpdate =
    15.0


type State
    = Playing
    | Over


type alias Model =
    { state : State
    , platforms : List GamePlatform.Model
    , avatar : Avatar.Model
    }


model : Model
model =
    { state = Playing
    , platforms = [ GamePlatform.model, GamePlatform.ground ]
    , avatar = Avatar.model
    }
