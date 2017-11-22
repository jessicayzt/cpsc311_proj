module GamePlatform exposing (..)

import Random exposing (Generator)
import Time exposing (Time, second)
import ViewUtil exposing (..)


ground : GamePlatform
ground =
    GamePlatform (width / 2) -ViewUtil.halfWidth (-ViewUtil.halfHeight + (ViewUtil.platformHeight / 2)) Spikes


init : GamePlatform
init =
    GamePlatform 200 (-ViewUtil.halfWidth / 2) (-ViewUtil.halfHeight / 2) Spikes


type alias GamePlatform =
    { w : Float
    , x : Float
    , y : Float
    , unit : Unit
    }


type alias PlatformToGenerate =
    { w : Float
    , heightDiff : Float
    , unit : Unit
    }


type Unit
    = Spikes
    | Waste
    | Health
    | TwoBones
    | ThreeBones
    | None


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
    Random.map assignUnit (Random.int 0 20)


assignUnit : Int -> Unit
assignUnit generated =
    if generated < 4 then
      Spikes
    else if generated < 7 then
      Waste
    else if generated < 11 then
      TwoBones
    else if generated < 14 then
      ThreeBones
    else if generated < 15 then
      Health
    else
      None

extendPlatforms : PlatformToGenerate -> List GamePlatform -> List GamePlatform
extendPlatforms newPlatform platforms =
    case List.head platforms of
        Just previousPlatform ->
            generatePlatform newPlatform previousPlatform :: platforms

        Nothing ->
            platforms


generatePlatform : PlatformToGenerate -> GamePlatform -> GamePlatform
generatePlatform newPlatform previousPlatform =
    let
        newPlatformY =
            previousPlatform.y + newPlatform.heightDiff

        groundExtension =
            GamePlatform (width / 2) (previousPlatform.x + ViewUtil.platformGap) (-ViewUtil.halfHeight + (ViewUtil.platformHeight / 2)) newPlatform.unit
    in
    if newPlatformY < (ViewUtil.halfHeight / 2) then
        GamePlatform newPlatform.w (previousPlatform.x + ViewUtil.platformGap) newPlatformY newPlatform.unit
    else
        groundExtension

removeCollectible: GamePlatform -> GamePlatform
removeCollectible prevPlatform  =
      GamePlatform prevPlatform.w prevPlatform.x prevPlatform.y None

hasCollectible: Maybe GamePlatform -> Bool
hasCollectible platform =
  case platform of
    Just platform ->
      case platform.unit of
        Health -> True
        TwoBones -> True
        ThreeBones -> True
        _ -> False
    Nothing ->
      False
