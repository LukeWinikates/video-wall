module GuideLines exposing (guideLines)

import App.Grid exposing (px, snap)
import Geometry exposing (Orientation, Scale, dimension)
import Model exposing (GridItem, Model)
import Html exposing (Attribute, Html, a, b, body, button, div, li, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, property, src, style)
import Set


-- TODO: only show the lines that are closest to the top, bottom, or center of the current thing
-- TODO: exclude the currently dragged thing? or maybe style it differently?
-- TODO: make the snapping more elegantly done


type alias Dimensionable =
    { left : Int, top : Int, scale : Scale, orientation : Orientation }


horizontalLineView : Int -> Html msg
horizontalLineView y =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "border-bottom", "1px dashed black" )
            , ( "width", "100vw" )
            , ( "top", y |> px )
            ]
        ]
        []


verticalLineView : Int -> Html msg
verticalLineView x =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "border-left", "1px dashed black" )
            , ( "height", "100vh" )
            , ( "left", x |> px )
            ]
        ]
        []


topCenterBottom : Dimensionable -> List Int
topCenterBottom item =
    let
        top =
            snap <| item.top

        height =
            snap <| .height <| dimension item.scale item.orientation
    in
        [ top, top + (height // 2), top + height ]


leftCenterRight : Dimensionable -> List Int
leftCenterRight item =
    let
        left =
            snap <| item.left

        width =
            snap <| .width <| dimension item.scale item.orientation
    in
        [ left, left + (width // 2), left + width ]


dimensionable : GridItem -> Dimensionable
dimensionable item =
    { left = item.left, top = item.top, scale = item.scale, orientation = item.orientation }


getHorizontals : Model -> List Int
getHorizontals model =
    model.movies
        |> List.map dimensionable
        |> List.map topCenterBottom
        |> List.concat
        |> Set.fromList
        |> Set.toList


getVerticals : Model -> List Int
getVerticals model =
    model.movies
        |> List.map dimensionable
        |> List.map leftCenterRight
        |> List.concat
        |> Set.fromList
        |> Set.toList


horizontals : Model -> List (Html msg)
horizontals model =
    List.map horizontalLineView (getHorizontals model)


verticals : Model -> List (Html msg)
verticals model =
    List.map verticalLineView (getVerticals model)


guideLines : Model -> List (Html msg)
guideLines model =
    List.append (verticals model) (horizontals model)
