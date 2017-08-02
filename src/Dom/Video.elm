module Dom.Video exposing (volume)

import Html exposing (Attribute)
import Html.Attributes exposing (property)
import Json.Encode


volume : Float -> Attribute msg
volume vol =
    (property "volume" (Json.Encode.string <| toString <| vol))
