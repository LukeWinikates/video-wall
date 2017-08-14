module App.Colors exposing (colors, transparentize, toCssColorString)

import Color exposing (Color)


colors =
    { hex =
        { thunder = "#3A3238"
        , platinum = "#E2E2E2"
        , graniteGray = "#636B61"
        , mistyRose = "#F5E3E0"
        }
    , color =
        { thunder = Color.rgb 58 50 56
        , platinum = Color.rgb 226 226 226
        , graniteGray = Color.rgb 99 107 97
        , mistyRose = Color.rgb 245 227 224
        }
    }


transparentize : Float -> Color -> Color
transparentize alpha color =
    Color.toRgb color |> \c -> { c | alpha = alpha } |> \c -> Color.rgba c.red c.green c.blue c.alpha


toCssColorString : Color -> String
toCssColorString color =
    color |> Color.toRgb |> \c -> "rgba(" ++ (c.red |> toString) ++ "," ++ (c.blue |> toString) ++ "," ++ (c.green |> toString) ++ ", " ++ (c.alpha |> toString) ++ ")"
