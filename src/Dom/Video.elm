module Dom.Video exposing (volume, playbackRate)

import Html exposing (Attribute)
import Html.Attributes exposing (property)
import Json.Encode


volume : Float -> Attribute msg
volume vol =
    (property "volume" (Json.Encode.string <| toString <| vol))


playbackRate : Float -> Attribute msg
playbackRate vol =
    (property "playbackRate" (Json.Encode.string <| toString <| vol))
