module App.SizePicker exposing (sizePickerView)

import App.Buttons exposing (changeButton, movieButton)
import App.Colors exposing (colors)
import App.Grid exposing (px, snap, videoBorderWidth)
import App.Msg exposing (Msg(..))
import FontAwesome
import Geometry exposing (Orientation(Horizontal, Vertical), Scale(..), dimension)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, onMouseOut, onMouseOver)
import Model exposing (GridContent(Initial, Picking), GridItem)
import Model.Mutate exposing (Mutation(ContentChange))


pickerButton : Orientation -> Scale -> Int -> String -> Html Msg
pickerButton orientation scale index content =
    movieButton
        [ onMouseEnter (ChangeItem (ContentChange (Initial (Just ( orientation, scale )))) index)
        , onMouseOut (ChangeItem (ContentChange (Initial Nothing)) index)
        , onClick (ChangeItem (ContentChange (Picking orientation scale)) index)
        ]
        [ text content ]


guideView : Orientation -> Scale -> Html Msg
guideView orientation scale =
    let
        { height, width } =
            dimension scale orientation
    in
        div
            [ style
                [ ( "border", "1px dashed black" )
                , ( "position", "absolute" )
                , ( "left", -10 |> snap |> px )
                , ( "width", width |> snap |> px )
                , ( "top", -10 |> snap |> px )
                , ( "height", height |> snap |> px )
                ]
            ]
            []


sizePickerView : GridItem -> Maybe ( Orientation, Scale ) -> Int -> Html Msg
sizePickerView item maybeGuide index =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "left", item.left |> snap |> px )
            , ( "width", 200 |> snap |> px )
            , ( "top", item.top |> snap |> px )
            , ( "height", 200 |> snap |> px )
            , ( "box-sizing", "border-box" )
            , ( "text-align", "center" )
            , ( "border", (videoBorderWidth |> px) ++ " solid " ++ colors.hex.thunder )
            , ( "border-radius", "2px" )
            , ( "background-color", colors.hex.thunder )
            , ( "padding", "10px" )
            , ( "overflow", "visible" )
            ]
        ]
        [ div
            []
            [ div [ style [ ( "text-align", "right" ) ] ] [ changeButton (Remove index) (FontAwesome.close colors.color.thunder 12) ]
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
        , Maybe.map (\( o, s ) -> guideView o s) maybeGuide |> Maybe.withDefault (Html.text "")
        ]
