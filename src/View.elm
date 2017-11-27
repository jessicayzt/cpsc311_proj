module View exposing (..)

import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Game.View as Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Model exposing (..)
import Text exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    let
        { width, height } =
            model.size
    in
    case model.screen of
        StartScreen ->
            body [ bodyStyle ]
                [ div [ divStyle ]
                    [ h3 [] [ Html.text "[ Press ENTER to begin ]" ]
                    , Html.text "You are playing as: "
                    , Html.input [ placeholder "Enter your name", value model.playerName, onInput InputName, inputStyle ] []
                    , Html.text "High Scores"
                    , highScoreTable model.highScores
                    ]
                ]

        _ ->
            Game.elementGame model.game
                |> container width height middle
                |> toHtml


highScoreTable : List HighScore -> Html Msg
highScoreTable highScores =
    div []
        [ table [ Html.Attributes.style [ ( "width", "100%" ) ] ]
            [ thead []
                [ tr []
                    [ th [ leftAlignColumn ] [ Html.text "Player" ]
                    , th [] [ Html.text "Score" ]
                    ]
                ]
            , tbody [] (List.map highScoreRow highScores)
            ]
        ]


highScoreRow : HighScore -> Html Msg
highScoreRow highScore =
    tr []
        [ td [ leftAlignColumn ] [ Html.text highScore.name ]
        , td []
            [ Html.text (toString highScore.score) ]
        ]


leftAlignColumn : Attribute msg
leftAlignColumn =
    Html.Attributes.style
        [ ( "text-align", "left" ) ]


bodyStyle : Attribute msg
bodyStyle =
    Html.Attributes.style
        [ ( "backgroundImage", "url(../graphic/env/startMenu.png)" )
        , ( "background-repeat", "no-repeat" )
        , ( "background-position", "center" )
        ]


inputStyle : Attribute msg
inputStyle =
    Html.Attributes.style
        [ ( "width", "100%" )
        , ( "height", "20px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "1.5em" )
        , ( "text-align", "center" )
        , ( "border", "none" )
        , ( "background-color", "rgba(0, 0, 0, 0)" )
        , ( "outline", "none" )
        , ( "color", "white" )
        ]


divStyle : Attribute msg
divStyle =
    Html.Attributes.style
        [ ( "width", "360px" )
        , ( "height", "200px" )
        , ( "font-size", "1.5em" )
        , ( "text-align", "center" )
        , ( "color", "orange" )
        , ( "top", "50%" )
        , ( "left", "50%" )
        , ( "position", "absolute" )
        , ( "margin", "-80px 0 0 -180px" )
        , ( "z-index", "15" )
        ]
