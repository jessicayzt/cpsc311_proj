module Model exposing (..)

import Game.Model as Game
import Window exposing (Size)


type Screen
    = StartScreen
    | PlayScreen
    | GameOverScreen


type alias HighScore =
    { name : String
    , score : Int
    }


type alias Model =
    { size : Size
    , game : Game.Model
    , screen : Screen
    , highScores : List HighScore
    , playerName : String
    }


initialModel : Model
initialModel =
    { size = Size 0 0
    , game = Game.model
    , screen = StartScreen
    , highScores = []
    , playerName = "Jack O'Lantern"
    }
