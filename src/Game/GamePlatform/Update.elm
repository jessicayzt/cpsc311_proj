module Game.GamePlatform.Update exposing (..)

import Game.GamePlatform.Model exposing (..)
import Random exposing (Generator)
import Time exposing (Time, second)
import ViewUtil exposing (..)


platformGenerator : Generator PlatformToGenerate
platformGenerator =
    Random.map3 PlatformToGenerate platformWidthGenerator platformHeightDiffGenerator unitGenerator


platformWidthGenerator : Generator Float
platformWidthGenerator =
    Random.float 100 400


platformHeightDiffGenerator : Generator Float
platformHeightDiffGenerator =
    Random.float 90 120


unitGenerator : Generator Unit
unitGenerator =
    Random.map assignUnit (Random.int 1 25)


assignUnit : Int -> Unit
assignUnit generated =
    if generated == 1 then
        Zombie ( 0.0, Right )
    else if generated <= 4 then
        Spikes
    else if generated <= 7 then
        Waste
    else if generated <= 11 then
        TwoBones
    else if generated <= 13 then
        Boost
    else if generated <= 14 then
        ThreeBones
    else if generated <= 15 then
        HP
    else
        None


extendPlatforms : PlatformToGenerate -> List Model -> List Model
extendPlatforms newPlatform platforms =
    case List.head platforms of
        Just previousPlatform ->
            generatePlatform newPlatform previousPlatform :: platforms

        Nothing ->
            platforms


generatePlatform : PlatformToGenerate -> Model -> Model
generatePlatform newPlatform previousPlatform =
    let
        newPlatformY =
            previousPlatform.y + newPlatform.heightDiff

        groundExtension =
            Model (width / 2) (previousPlatform.x + ViewUtil.platformGap) (-ViewUtil.halfHeight + (ViewUtil.platformHeight / 2)) newPlatform.unit
    in
    if newPlatformY < (ViewUtil.halfHeight / 2) then
        Model newPlatform.w (previousPlatform.x + ViewUtil.platformGap) newPlatformY newPlatform.unit
    else
        groundExtension


hasCollectible : Maybe Model -> Bool
hasCollectible platform =
    case platform of
        Just platform ->
            case platform.unit of
                Boost ->
                    True

                HP ->
                    True

                TwoBones ->
                    True

                ThreeBones ->
                    True

                _ ->
                    False

        Nothing ->
            False


removeCollectible : Model -> Model
removeCollectible prevPlatform =
    Model prevPlatform.w prevPlatform.x prevPlatform.y None
