module ViewUtil exposing (..)


width : Float
width =
    1000


halfWidth : Float
halfWidth =
    width / 2


twiceWidth : Float
twiceWidth =
    width * 2


height : Float
height =
    570


halfHeight : Float
halfHeight =
    height / 2


platformHeight : Float
platformHeight =
    40


platformBuffer : Float
platformBuffer =
    70


leftBuffer : Float
leftBuffer =
    100


left : Float
left =
    -halfWidth + leftBuffer


onGround : Float
onGround =
    -halfHeight + (platformHeight / 2) + platformBuffer


platformGap : Float
platformGap =
    150


pit : Float
pit =
    -halfHeight - platformBuffer
