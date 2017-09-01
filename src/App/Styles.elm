module App.Styles exposing (..)

import App.Colors exposing (colors)
import Css exposing (..)
import Css.Elements exposing (body)
import Css.Namespace exposing (namespace)


css =
    stylesheet
        [ body
            [ fontFamilies [ "Lato", "sans-serif" ]
            , boxSizing borderBox
            , backgroundColor (hex colors.hex.graniteGray)
            ]
        ]


compiled =
    (.css (Css.compile [ css ]))
