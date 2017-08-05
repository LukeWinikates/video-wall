module App.Tray exposing (menuView)

import App.Buttons exposing (changeButton)
import App.Colors exposing (colors)
import App.Grid exposing (px)
import App.Msg exposing (Msg(ChangeCollection, TrayMenu))
import FontAwesome
import Html exposing (Html, a, div, h2)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (TrayMode(Collapsed, Expanded))
import Movie exposing (MovieCollection)


collectionSwitchLink : MovieCollection -> Html Msg
collectionSwitchLink collection =
    div []
        [ a [ onClick (ChangeCollection collection) ]
            [ Html.text collection.title
            ]
        ]


menuView : TrayMode -> Html Msg
menuView mode =
    case mode of
        Collapsed ->
            div
                [ style [ ( "position", "absolute" ), ( "top", 20 |> px ), ( "right", 20 |> px ) ] ]
                [ changeButton (TrayMenu Expanded) (FontAwesome.toggle_left colors.color.thunder 16) ]

        Expanded ->
            div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", 0 |> px )
                    , ( "right", 0 |> px )
                    , ( "padding", 20 |> px )
                    , ( "height", "100vh" )
                    , ( "width", "400px" )
                    , ( "background-color", colors.hex.mistyRose )
                    , ( "border-left", "2px solid " ++ colors.hex.thunder )
                    ]
                ]
                ([ changeButton (TrayMenu Collapsed) (FontAwesome.toggle_right colors.color.thunder 16)
                 , h2 [] [ Html.text "Collections" ]
                 ]
                    ++ (List.map collectionSwitchLink Movie.collections)
                )
