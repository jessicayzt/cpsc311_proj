module Game.View exposing (..)

import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Game.Avatar.View as Avatar
import Game.GamePlatform.View as GamePlatform
import Game.Model exposing (..)
import Html exposing (Html)
import String exposing (..)
import Text exposing (..)
import ViewUtil exposing (..)


width : Int
width =
    truncate ViewUtil.width


height : Int
height =
    truncate ViewUtil.height


content : Model -> String
content game =
    "HP : "
        ++ toString game.avatar.hp
        ++ "\nSPEED X "
        ++ toString game.avatar.speed.multiplier
        ++ "\nSCORE : "
        ++ scoreString game.avatar.score


textElement : Model -> Element
textElement game =
    fromString (content game)
        |> Text.color Color.white
        |> formatText


scoreString : Int -> String
scoreString score =
    let
        scoreString =
            toString score
    in
    repeat (6 - length scoreString) "0" ++ scoreString


elementGame : Model -> Element
elementGame game =
    let
        background =
            image width height "../graphic/env/background.png"

        textX =
            -halfWidth + 150

        textY =
            halfHeight - 80

        overlayForms =
            GamePlatform.platformForms game.platforms
                ++ GamePlatform.platformUnitForms game.platforms
                ++ [ Avatar.avatarForm game.avatar
                        |> move ( game.avatar.x, game.avatar.y )
                   , textElement game
                        |> toForm
                        |> move ( textX, textY )
                   ]

        toRender =
            toForm background :: overlayForms
    in
    if game.state == Game.Model.Over then
        collage width
            height
            (toRender ++ [ gameOverForm ])
    else
        collage width
            height
            toRender


gameOverForm : Form
gameOverForm =
    Text.fromString
        "Game Over!\nPress F5 to play again."
        |> Text.color Color.orange
        |> formatText
        |> toForm


formatText : Text -> Element
formatText text =
    text
        |> bold
        |> Text.height 30
        |> monospace
        |> leftAligned
