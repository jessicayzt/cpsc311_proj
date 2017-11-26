module View exposing (..)

import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Game.Avatar.Model as Avatar
import Game.GamePlatform.Model as GamePlatform
import Game.Model as Game
import Game.Update as Game
import Html exposing (Html)
import Model exposing (..)
import String exposing (..)
import Text exposing (..)
import Update exposing (..)
import ViewUtil exposing (..)


width : Int
width =
    truncate ViewUtil.width


height : Int
height =
    truncate ViewUtil.height


view : Model -> Html Msg
view model =
    let
        { width, height } =
            model.game.size

        view =
            elementGame model.game
    in
    view
        |> container width height middle
        |> toHtml


elementGame : Game.Model -> Element
elementGame game =
    let
        background =
            image width height "../graphic/env/background.png"

        textX =
            -halfWidth + 150

        textY =
            halfHeight - 80

        overlayForms =
            platformForms game.platforms
                ++ platformUnitForms game.platforms
                ++ [ avatarForm game
                        |> move ( game.avatar.x, game.avatar.y )
                   , textElement game
                        |> toForm
                        |> move ( textX, textY )
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


platformForms : List GamePlatform.Model -> List Form
platformForms platforms =
    List.map platformForm platforms


platformForm : GamePlatform.Model -> Form
platformForm platform =
    let
        platRect =
            rect platform.w ViewUtil.platformHeight
                |> filled Color.darkGrey
    in
    platRect |> move ( platform.x, platform.y )


platformUnitForms : List GamePlatform.Model -> List Form
platformUnitForms platforms =
    List.map platformUnitForm (List.filter (\platform -> platform.unit /= GamePlatform.None) platforms)


platformUnitForm : GamePlatform.Model -> Form
platformUnitForm platform =
    let
        unitWidth =
            50

        unitHeight =
            50

        unit =
            if platform.unit == GamePlatform.Spikes then
                "hazard/spikes"
            else if platform.unit == GamePlatform.Waste then
                "hazard/nuclear_waste"
            else if platform.unit == GamePlatform.HP then
                "collectible/hp"
            else if platform.unit == GamePlatform.TwoBones then
                "collectible/bones_2"
            else if platform.unit == GamePlatform.Boost then
                "collectible/boost"
            else
                "collectible/bones_3"

        src =
            "../graphic/env/" ++ unit ++ ".png"

        element =
            image unitWidth unitHeight src
    in
    element
        |> toForm
        |> move ( platform.x, platform.y + 40 )


avatarForm : Game.Model -> Form
avatarForm game =
    let
        avatarWidth =
            116

        avatarHeight =
            153

        rightwalk =
            "../graphic/avatar/walk/right.gif"

        leftwalk =
            "../graphic/avatar/walk/left.gif"

        rightjump =
            "../graphic/avatar/jump/right.gif"

        leftjump =
            "../graphic/avatar/jump/left.gif"

        rightidle =
            "../graphic/avatar/idle/right.gif"

        leftidle =
            "../graphic/avatar/idle/left.gif"

        rightdie =
            "../graphic/avatar/die/right.gif"

        leftdie =
            "../graphic/avatar/die/left.gif"
    in
    if game.avatar.hp <= 0 then
        case game.avatar.dir of
            Avatar.Left ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 1 (toForm (image avatarHeight avatarHeight rightdie)))
                    )

            Avatar.Right ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 1 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                    )
    else if game.avatar.vy /= 0 then
        case game.avatar.dir of
            Avatar.Left ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 1 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                    )

            Avatar.Right ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 1 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                    )
    else if game.avatar.vx /= 0 then
        if game.avatar.speed.multiplier >= 1.5 then
            case game.avatar.dir of
                Avatar.Left ->
                    group
                        (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                            :: alpha 1 (toForm (image avatarWidth avatarHeight leftwalk))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                            :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                            :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                        )

                Avatar.Right ->
                    group
                        (alpha 1 (toForm (image avatarWidth avatarHeight rightwalk))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                            :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                            :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                        )
        else
            case game.avatar.dir of
                Avatar.Left ->
                    group
                        (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                            :: alpha 1 (toForm (image avatarWidth avatarHeight leftwalk))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                            :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                            :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                        )

                Avatar.Right ->
                    group
                        (alpha 1 (toForm (image avatarWidth avatarHeight rightwalk))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                            :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                            :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                        )
    else
        case game.avatar.dir of
            Avatar.Left ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 1 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                    )

            Avatar.Right ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 1 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 0 (toForm (image avatarHeight avatarHeight rightdie)))
                    )


textElement : Game.Model -> Element
textElement game =
    Text.fromString (content game)
        |> Text.color Color.white
        |> formatText


content : Game.Model -> String
content game =
    "HP : "
        ++ toString game.avatar.hp
        ++ "\nSPEED X "
        ++ toString game.avatar.speed.multiplier
        ++ "\nSCORE : "
        ++ scoreString game.avatar.score


scoreString : Int -> String
scoreString score =
    let
        scoreString =
            toString score
    in
    repeat (6 - length scoreString) "0" ++ scoreString


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
