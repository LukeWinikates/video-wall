module App.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body)
import Css.Namespace exposing (namespace)


css =
    stylesheet
        [ body
            [ fontFamilies [ "Lato", "sans-serif" ]
            , boxSizing borderBox
            ]
        ]


compiled =
    (.css (Css.compile [ css ]))
