module App.SizePicker exposing (sizePickerView)

import App.Buttons exposing (changeButton, movieButton)
import App.Colors exposing (colors)
import App.Grid exposing (px, snap, videoBorderWidth)
import App.Msg exposing (Msg(..))
import FontAwesome
import Geometry exposing (Orientation(Horizontal, Vertical), Scale(..), dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Model exposing (GridContent(Initial, Picking), GridItem)
import Model.Mutate exposing (Mutation(ContentChange))


pickerButton : Orientation -> Scale -> Int -> String -> Html Msg
pickerButton orientation scale index content =
    movieButton
        [ onMouseEnter (ChangeItem (ContentChange (Initial (Just ( orientation, scale )))) index)
        , onMouseLeave (ChangeItem (ContentChange (Initial Nothing)) index)
        , onClick (ChangeItem (ContentChange (Picking orientation scale)) index)
        ]
        [ text content ]


sizePickerView : GridItem -> Maybe ( Orientation, Scale ) -> Int -> Html Msg
sizePickerView item maybeGuide index =
    div
        [ style
            ([ ( "position", "absolute" )
             , ( "left", item.left |> snap |> px )
             , ( "top", item.top |> snap |> px )
             , ( "box-sizing", "border-box" )
             , ( "text-align", "center" )
             ]
            )
        ]
        [ div
            [ style
                ((Maybe.map
                    (\( o, s ) ->
                        dimension s o
                            |> (\{ height, width } ->
                                    [ ( "border", "5px dashed black" )
                                    , ( "box-sizing", "border-box" )
                                    , ( "width", width |> snap |> px )
                                    , ( "height", height |> snap |> px )
                                    ]
                               )
                    )
                    maybeGuide
                 )
                    |> Maybe.withDefault []
                )
            ]
            []
        , div
            [ style
                [ ( "border", "5px dashed black" )
                , ( "border-radius", "2px" )
                , ( "box-sizing", "border-box" )
                , ( "padding", "5px" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "position", "absolute" )
                , ( "background-color", colors.hex.graniteGray )
                , ( "width", 200 |> snap |> px )
                , ( "height", 200 |> snap |> px )
                ]
            ]
            [ div [ style [ ( "text-align", "right" ) ] ]
                [ changeButton (Remove index) (FontAwesome.close colors.color.thunder 12) ]
            , div [ style [ ( "text-align", "left" ) ] ] [ (Html.text "vertical") ]
            , div []
                [ pickerButton Vertical Small index "S"
                , pickerButton Vertical Medium index "M"
                , pickerButton Vertical Large index "L"
                ]
            , div [ style [ ( "text-align", "left" ) ] ] [ (Html.text "horizontal") ]
            , div
                []
                [ pickerButton Horizontal Small index "S"
                , pickerButton Horizontal Medium index "M"
                , pickerButton Horizontal Large index "L"
                ]
            ]
        ]
