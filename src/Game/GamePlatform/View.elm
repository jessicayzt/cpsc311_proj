module Game.GamePlatform.View exposing (..)

import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Game.GamePlatform.Model exposing (..)
import Html exposing (Html)
import ViewUtil exposing (..)


platformForms : List Model -> List Form
platformForms platforms =
    List.map platformForm platforms


platformForm : Model -> Form
platformForm platform =
    let
        platRect =
            rect platform.w ViewUtil.platformHeight
                |> filled Color.darkGrey
    in
    platRect |> move ( platform.x, platform.y )


platformUnitForms : List Model -> List Form
platformUnitForms platforms =
    List.map platformUnitForm (List.filter (\platform -> platform.unit /= None) platforms)


platformUnitForm : Model -> Form
platformUnitForm platform =
    let
        unitWidth =
            50

        unitHeight =
            50

        unit =
            case platform.unit of
                Spikes ->
                    "hazard/spikes"

                Waste ->
                    "hazard/nuclear_waste"

                Zombie status ->
                    if Tuple.second status == Left then
                        "hazard/zombie/left"
                    else
                        "hazard/zombie/right"

                HP ->
                    "collectible/hp"

                TwoBones ->
                    "collectible/bones_2"

                Boost ->
                    "collectible/boost"

                _ ->
                    "collectible/bones_3"

        src =
            "../graphic/env/" ++ unit

        element =
            case platform.unit of
                Zombie status ->
                    image 108 130 (src ++ ".gif")

                _ ->
                    image unitWidth unitHeight (src ++ ".png")

        elementX =
            case platform.unit of
                Zombie status ->
                    platform.x + Tuple.first status

                _ ->
                    platform.x

        elementY =
            case platform.unit of
                Zombie status ->
                    platform.y + 70

                _ ->
                    platform.y + 40
    in
    element
        |> toForm
        |> move ( elementX, elementY )
