module App.Tray exposing (menuView)

import App.Buttons exposing (changeButton)
import App.Colors exposing (colors)
import App.Grid exposing (px)
import App.Msg exposing (Msg(ChangeCollection, TrayMenu))
import FontAwesome
import Html exposing (Html, a, div, h2)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Model exposing (Model, TrayContent(MoviePicker, ShowingPoem), TrayMode(Collapsed, Expanded))
import Movie exposing (MovieCollection)
import Dom.ZIndexes as ZIndexes


collectionSwitchLink : MovieCollection -> Html Msg
collectionSwitchLink collection =
    div []
        [ a [ onClick (ChangeCollection collection) ]
            [ Html.text collection.title
            ]
        ]


shouldShowTrayButton : Model -> Bool
shouldShowTrayButton model =
    model.lastInteractionTime > model.lastTick - 1500


menuView : Model -> Html Msg
menuView model =
    case model.trayMode of
        Collapsed ->
            div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", 20 |> px )
                    , ( "right", 20 |> px )
                    , ( "z-index", ZIndexes.topmost |> toString )
                    ]
                ]
                [ if Model.userHasInteractedRecently model then
                    changeButton (TrayMenu (Expanded ShowingPoem)) (FontAwesome.toggle_left colors.color.thunder 16)
                  else
                    Html.text ""
                ]

        Expanded _ ->
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
                    , ( "box-sizing", "border-box" )
                    ]
                ]
                ([ changeButton (TrayMenu Collapsed) (FontAwesome.toggle_right colors.color.thunder 16)
                 , h2 [] [ Html.text "Collections" ]
                 ]
                    ++ (List.map collectionSwitchLink Movie.collections)
                    ++ [ h2 [] [ Html.text "Videos" ]
                       , a [ onClick (TrayMenu (Expanded (MoviePicker { highlighted = Nothing }))) ] [ Html.text "Add +" ]
                       ]
                )
