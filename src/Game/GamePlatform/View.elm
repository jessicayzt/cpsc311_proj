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
            if platform.unit == Spikes then
                "hazard/spikes"
            else if platform.unit == Waste then
                "hazard/nuclear_waste"
            else if platform.unit == HP then
                "collectible/hp"
            else if platform.unit == TwoBones then
                "collectible/bones_2"
            else if platform.unit == Boost then
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
