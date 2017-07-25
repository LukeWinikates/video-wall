module GuideLines exposing (guideLines)

import DomHelpers exposing (px, snap)
import Model exposing (Model, GridMovie)
import Html exposing (Attribute, Html, a, b, body, button, div, li, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, property, src, style)
import Set


-- TODO: only show the lines that are closest to the top, bottom, or center of the current thing
-- TODO: exclude the currently dragged thing?


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


topCenterBottom : GridMovie -> List Int
topCenterBottom gridMovie =
    [ gridMovie.top, gridMovie.top + (gridMovie.height // 2), gridMovie.top + gridMovie.height ]


leftCenterRight : GridMovie -> List Int
leftCenterRight gridMovie =
    [ gridMovie.left, gridMovie.left + (gridMovie.width // 2), gridMovie.left + gridMovie.width ]


getHorizontals : Model -> List Int
getHorizontals model =
    List.map topCenterBottom model.movies
        |> List.concat
        |> Set.fromList
        |> Set.toList


getVerticals : Model -> List Int
getVerticals model =
    List.map leftCenterRight model.movies
        |> List.concat
        |> List.map snap
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
