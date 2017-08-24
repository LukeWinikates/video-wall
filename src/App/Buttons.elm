module App.Buttons exposing (movieButton, changeButton, dragButton)

import App.Colors exposing (colors)
import Html exposing (Attribute, Html, button)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode
import Mouse


onMouseDownWithDecoder : (Mouse.Position -> msg) -> Attribute msg
onMouseDownWithDecoder f =
    on "mousedown" (Json.Decode.map f Mouse.position)


movieButton : List (Attribute msg) -> List (Html msg) -> Html msg
movieButton attributes content =
    button
        (attributes
            ++ [ style
                    [ ( "background-color", colors.hex.mistyRose )
                    , ( "border-radius", "2px" )
                    , ( "color", colors.hex.thunder )
                    , ( "min-width", "3em" )
                    , ( "font-weight", "bold" )
                    , ( "height", "24px" )
                    , ( "border", "none" )
                    , ( "cursor", "pointer" )
                    , ( "margin", "5px" )
                    , ( "padding", "5px 10px" )
                    ]
               ]
        )
        content


changeButton : msg -> Html msg -> Html msg
changeButton msg content =
    movieButton
        [ onClick msg ]
        [ content ]


dragButton : (Mouse.Position -> msg) -> Html msg -> Html msg
dragButton msg icon =
    movieButton
        [ onMouseDownWithDecoder msg ]
        [ icon ]
