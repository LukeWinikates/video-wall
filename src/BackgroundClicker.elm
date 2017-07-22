module BackgroundClicker exposing (decodePosition, onClickElementWithId)

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)
import Json.Decode exposing (Decoder)
import Mouse exposing (Position)


decodePosition : Decoder Position
decodePosition =
    Json.Decode.map2
        Position
        (Json.Decode.field "pageX" Json.Decode.int)
        (Json.Decode.field "pageY" Json.Decode.int)


onClickElementWithId : String -> Decoder a -> (a -> msg) -> Attribute msg
onClickElementWithId intendedId newDecoder f =
    onWithOptions "click"
        Html.Events.defaultOptions
        ((Json.Decode.at [ "target", "id" ] Json.Decode.string)
            |> (Json.Decode.andThen
                    (\id ->
                        if id == intendedId then
                            Json.Decode.map f newDecoder
                        else
                            Json.Decode.fail "abort click handler"
                    )
               )
        )
