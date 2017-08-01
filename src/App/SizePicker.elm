module App.SizePicker exposing (sizePickerView)

import App.Buttons exposing (changeButton)
import App.Colors exposing (colors)
import App.Grid exposing (px, snap, videoBorderWidth)
import App.Msg exposing (Msg(..))
import FontAwesome
import Geometry exposing (Orientation(Horizontal, Vertical), Scale(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Model exposing (GridContent(Picking), GridItem)
import Model.Mutate exposing (Mutation(ContentChange))


sizePickerView : GridItem -> Int -> Html Msg
sizePickerView item index =
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
            , ( "padding", "10px" )
            ]
        ]
        [ div
            []
            [ div [ style [ ( "text-align", "right" ) ] ] [ changeButton (Remove index) (FontAwesome.close colors.color.thunder 12) ]
            , div [ style [ ( "text-align", "left" ) ] ] [ (Html.text "vertical") ]
            , div []
                [ changeButton (ChangeMovie (ContentChange (Picking Vertical Small)) index) (text "S")
                , changeButton (ChangeMovie (ContentChange (Picking Vertical Medium)) index) (text "M")
                , changeButton (ChangeMovie (ContentChange (Picking Vertical Large)) index) (text "L")
                ]
            , div [ style [ ( "text-align", "left" ) ] ] [ (Html.text "horizontal") ]
            , div
                []
                [ changeButton (ChangeMovie (ContentChange (Picking Horizontal Small)) index) (text "S")
                , changeButton (ChangeMovie (ContentChange (Picking Horizontal Medium)) index) (text "M")
                , changeButton (ChangeMovie (ContentChange (Picking Horizontal Large)) index) (text "L")
                ]
            ]
        ]
