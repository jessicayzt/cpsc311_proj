module View exposing (..)

import Avatar exposing (..)
import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Game exposing (..)
import GamePlatform exposing (..)
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


view : Game -> Html Msg
view game =
    let
        { width, height } =
            game.size

        view =
            elementGame game
    in
    view
        |> container width height middle
        |> toHtml


elementGame : Game -> Element
elementGame game =
    let
        background =
            image width height "../graphic/env/background.png"

        uiX =
            -halfWidth + 90

        uiY =
            halfHeight - 80

        overlayForms =
            platformForms game.platforms
                ++ platformUnitForms game.platforms
                ++ [ avatarElement game
                        |> toForm
                        |> move ( game.avatar.x, game.avatar.y )
                   , uiElement game
                        |> toForm
                        |> move ( uiX, uiY )
                   ]

        toRender =
            toForm background :: overlayForms
    in
    if game.state == Game.Over then
        collage width
            height
            (toRender ++ [ gameOverForm ])
    else
        collage width
            height
            toRender


platformForms : List GamePlatform -> List Form
platformForms platforms =
    List.map platformForm platforms


platformForm : GamePlatform -> Form
platformForm platform =
    let
        platRect =
            rect platform.w ViewUtil.platformHeight
                |> filled Color.darkGrey
    in
    platRect |> move ( platform.x, platform.y )


platformUnitForms : List GamePlatform -> List Form
platformUnitForms platforms =
    List.map platformUnitForm (List.filter (\platform -> platform.unit /= None) platforms)


platformUnitForm : GamePlatform -> Form
platformUnitForm platform =
    let
        unitWidth =
            50

        unitHeight =
            50

        unit =
            if platform.unit == Spikes then
                "spikes"
            else
                "nuclear_waste"

        src =
            "../graphic/env/" ++ unit ++ ".png"

        element =
            image unitWidth unitHeight src
    in
    element
        |> toForm
        |> move ( platform.x, platform.y + 40 )


avatarElement : Game -> Element
avatarElement game =
    let
        avatarWidth =
            116

        avatarHeight =
            153

        verb =
            if game.avatar.hp <= 0 then
                "die"
            else if onPlatform game.avatar game.platforms /= True then
                "jump"
            else if game.avatar.vx /= 0 then
                if game.avatar.speedMultiplier >= 1.5 then
                    "run"
                else
                    "walk"
            else
                "idle"

        dir =
            case game.avatar.dir of
                Left ->
                    "left"

                Right ->
                    "right"

        src =
            "../graphic/avatar/" ++ verb ++ "/" ++ dir ++ ".gif"
    in
    if verb == "die" then
        image avatarHeight avatarHeight src
    else
        image avatarWidth avatarHeight src


uiElement : Game -> Element
uiElement game =
    Text.fromString (uiContent game)
        |> Text.color Color.white
        |> formatText


uiContent : Game -> String
uiContent game =
    "HP : "
        ++ toString game.avatar.hp
        ++ "\nSPEED X "
        ++ toString game.avatar.speedMultiplier
        ++ "\nSCORE : "
        ++ toString game.score


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
        |> Text.height 35
        |> typeface [ "chiller", "courier" ]
        |> leftAligned
