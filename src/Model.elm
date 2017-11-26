module Model exposing (..)

import Game.Model


type alias Model =
    { game : Game.Model.Model }


initialModel : Model
initialModel =
    { game = Game.Model.model }
