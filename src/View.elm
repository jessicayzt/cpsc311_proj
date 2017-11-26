module View exposing (..)

import Color exposing (Color)
import Element exposing (..)
import Game.Model as Game
import Game.View as Game
import Html exposing (Html)
import Model exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    let
        { width, height } =
            model.game.size

        view =
            Game.elementGame model.game
    in
    view
        |> container width height middle
        |> toHtml
