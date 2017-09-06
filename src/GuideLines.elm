module GuideLines exposing (guideLinesView)

import App.Grid exposing (px, snap)
import Dom.Dragging
import Geometry exposing (Orientation, Scale, dimension)
import List.Extra
import Model exposing (GridItem, Model)
import Html exposing (Attribute, Html, a, b, body, button, div, li, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, property, src, style)
import Set


-- TODO: exclude the currently dragged thing? or maybe style it differently?


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
            , ( "top", "0" )
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


getHorizontals : Model -> GridItem -> List Int
getHorizontals model item =
    model.movies
        |> List.map dimensionable
        |> List.map topCenterBottom
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> List.sortBy (nearestDistance <| topCenterBottom <| dimensionable item)
        |> List.take 9


getVerticals : Model -> GridItem -> List Int
getVerticals model item =
    model.movies
        |> List.map dimensionable
        |> List.map leftCenterRight
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> List.sortBy (nearestDistance <| leftCenterRight <| dimensionable item)
        |> List.take 9


nearestDistance : List Int -> Int -> Int
nearestDistance items pos =
    items
        |> List.map ((((-) pos) >> abs))
        |> List.minimum
        |> Maybe.withDefault 0


horizontals : Model -> GridItem -> List (Html msg)
horizontals model item =
    List.map horizontalLineView (getHorizontals model item)


verticals : Model -> GridItem -> List (Html msg)
verticals model item =
    List.map verticalLineView (getVerticals model item)


guideLines : Model -> GridItem -> List (Html msg)
guideLines model item =
    (verticals model item)
        ++ (horizontals model item)


guideLinesView : Model -> List (Html msg)
guideLinesView model =
    model.dragging
        |> Maybe.andThen (\{ state } -> (List.Extra.getAt state model.movies))
        |> Maybe.map (\item -> guideLines model item)
        |> Maybe.withDefault []
