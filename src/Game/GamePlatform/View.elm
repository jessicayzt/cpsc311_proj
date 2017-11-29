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
    in
    case platform.unit of
        Zombie status ->
            let
                zombieX =
                    platform.x + Tuple.first status

                zombieY =
                    platform.y + 70

                zombieLeftForm =
                    toForm (image 108 130 "../graphic/env/hazard/zombie/left.gif")

                zombieRightForm =
                    toForm (image 108 130 "../graphic/env/hazard/zombie/right.gif")
            in
            if Tuple.second status == Left then
                group
                    (alpha 1 zombieLeftForm
                        :: List.singleton (alpha 0 zombieRightForm)
                    )
                    |> move ( zombieX, zombieY )
            else
                group
                    (alpha 0 zombieLeftForm
                        :: List.singleton (alpha 1 zombieRightForm)
                    )
                    |> move ( zombieX, zombieY )

        _ ->
            image unitWidth unitHeight (src ++ ".png")
                |> toForm
                |> move ( platform.x, platform.y + 40 )
