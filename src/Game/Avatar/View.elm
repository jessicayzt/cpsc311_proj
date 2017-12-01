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

        walk_left =
            "../graphic/avatar/walk/left.gif"

        walk_right =
            "../graphic/avatar/walk/right.gif"

        run_left =
            "../graphic/avatar/run/left.gif"

        run_right =
            "../graphic/avatar/run/right.gif"

        jump_left =
            "../graphic/avatar/jump/left.gif"

        jump_right =
            "../graphic/avatar/jump/right.gif"

        die_left =
            "../graphic/avatar/die/left.gif"

        die_right =
            "../graphic/avatar/die/right.gif"

        idle_left =
            "../graphic/avatar/idle/left.gif"

        idle_right =
            "../graphic/avatar/idle/right.gif"
    in
    if avatar.hp <= 0 then
        case avatar.dir of
            Left ->
                group
                    (alpha 0 (toForm (image avatarHeight avatarHeight die_right))
                        :: List.singleton (alpha 1 (toForm (image avatarHeight avatarHeight die_left)))
                    )

            Right ->
                group
                    (alpha 0 (toForm (image avatarHeight avatarHeight die_left))
                        :: List.singleton (alpha 1 (toForm (image avatarHeight avatarHeight die_right)))
                    )
    else if avatar.vy /= 0 then
        case avatar.dir of
            Left ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight idle_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                        :: List.singleton (alpha 1 (toForm (image avatarWidth avatarHeight jump_left)))
                    )

            Right ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight idle_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                        :: List.singleton (alpha 1 (toForm (image avatarWidth avatarHeight jump_right)))
                    )
    else if avatar.vx /= 0 then
        if avatar.speed.multiplier >= 1.5 then
            case avatar.dir of
                Left ->
                    group
                        (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                            :: alpha 1 (toForm (image avatarWidth avatarHeight run_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                            :: List.singleton (alpha 0 (toForm (image avatarWidth avatarHeight idle_right)))
                        )

                Right ->
                    group
                        (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                            :: alpha 1 (toForm (image avatarWidth avatarHeight run_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                            :: List.singleton (alpha 0 (toForm (image avatarWidth avatarHeight idle_right)))
                        )
        else
            case avatar.dir of
                Left ->
                    group
                        (alpha 1 (toForm (image avatarWidth avatarHeight walk_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                            :: List.singleton (alpha 0 (toForm (image avatarWidth avatarHeight idle_right)))
                        )

                Right ->
                    group
                        (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                            :: alpha 1 (toForm (image avatarWidth avatarHeight walk_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                            :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                            :: List.singleton (alpha 0 (toForm (image avatarWidth avatarHeight idle_right)))
                        )
    else
        case avatar.dir of
            Left ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                        :: alpha 1 (toForm (image avatarWidth avatarHeight idle_left))
                        :: List.singleton (alpha 0 (toForm (image avatarWidth avatarHeight idle_right)))
                    )

            Right ->
                group
                    (alpha 0 (toForm (image avatarWidth avatarHeight walk_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight walk_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight run_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight jump_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight jump_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_left))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight die_right))
                        :: alpha 0 (toForm (image avatarWidth avatarHeight idle_left))
                        :: List.singleton (alpha 1 (toForm (image avatarWidth avatarHeight idle_right)))
                    )
