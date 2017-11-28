module Game.Avatar.View exposing (..)

import Collage exposing (..)
import Element exposing (..)
import Game.Avatar.Model exposing (..)
import Html exposing (Html)


avatarForm : Model -> Form
avatarForm avatar =
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
    if avatar.hp <= 0 then
        case avatar.dir of
            Left ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight rightwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftwalk))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftjump))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight rightidle))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight leftidle))
                        :: alpha 0 (toForm (image avatarHeight avatarHeight rightdie))
                        :: List.singleton (alpha 1 (toForm (image avatarHeight avatarHeight leftdie)))
                    )

            Right ->
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
    else if avatar.vy > 0 then
        case avatar.dir of
            Left ->
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

            Right ->
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
    else if avatar.vx /= 0 then
        if avatar.speed.multiplier >= 1.5 then
            case avatar.dir of
                Left ->
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

                Right ->
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
            case avatar.dir of
                Left ->
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

                Right ->
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
        case avatar.dir of
            Left ->
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

            Right ->
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
