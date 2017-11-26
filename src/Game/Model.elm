module Game.Model exposing (..)

import Game.Avatar.Model as Avatar
import Game.GamePlatform.Model as GamePlatform
import Task exposing (..)
import Window exposing (Size)


type State
    = Playing
    | Over


type alias Model =
    { size : Size
    , state : State
    , platforms : List GamePlatform.Model
    , avatar : Avatar.Model
    }


model : Model
model =
    { size = Size 0 0
    , state = Playing
    , platforms = [ GamePlatform.model, GamePlatform.ground ]
    , avatar = Avatar.model
    }
