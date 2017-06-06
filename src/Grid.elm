module Grid exposing (append, GridRectangle, appendAll, forType)

import List exposing (foldl)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Dict exposing (Dict)
import Maybe exposing (withDefault)


type alias Size =
    ( Int, Int )


type alias GridRectangle =
    { bottomRow : Int
    , topRow : Int
    , leftColumn : Int
    , rightColumn : Int
    }


type alias Sizer a =
    a -> Size


type alias Point =
    { x : Int, y : Int }


type alias Grid a =
    { sizer : Sizer a
    , width : Int
    , height : Int
    , items : List ( a, GridRectangle )
    , edges : Edges
    }


type alias Edges =
    Dict Int Int


emptyEdges =
    Dict.empty


insertEdge : Point -> Edges -> Edges
insertEdge point edges =
    Dict.insert point.x point.y edges


edgePoints : Edges -> List Point
edgePoints e =
    Dict.toList e |> List.map (\( x, y ) -> { x = x, y = y })


appendAll : Grid a -> List a -> Grid a
appendAll g items =
    (foldl append g items)


forType : (a -> Size) -> Int -> Int -> Grid a
forType sizer x y =
    { sizer = sizer, width = x, height = y, items = [], edges = insertEdge { x = 1, y = 1 } emptyEdges }


fromBasePoint : Point -> Size -> GridRectangle
fromBasePoint point ( width, height ) =
    { leftColumn = point.x
    , rightColumn = point.x + width
    , topRow = point.y
    , bottomRow = point.y + height
    }


fits : Grid a -> GridRectangle -> Bool
fits grid rect =
    grid.height > rect.bottomRow


nextRootPos : Grid a -> Size -> Maybe GridRectangle
nextRootPos g s =
    (edgePoints g.edges) |> List.map (\p -> fromBasePoint p s) |> List.filter (fits g) |> List.head


append : a -> Grid a -> Grid a
append frame grid =
    nextRootPos grid (grid.sizer frame)
        |> Maybe.map
            (\newRect ->
                { grid
                    | items = grid.items ++ [ ( frame, newRect ) ]
                    , edges =
                        grid.edges
                            |> insertEdge { x = newRect.leftColumn, y = newRect.bottomRow }
                            |> insertEdge { x = newRect.rightColumn, y = 1 }
                }
            )
        |> withDefault grid
