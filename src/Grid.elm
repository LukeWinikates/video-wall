module Grid exposing (append, GridRectangle, appendAll, forType, Size)

import List exposing (foldl)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Maybe exposing (withDefault)
import Edges exposing (Edges, insert, single, toPointList)
import Geometry exposing (Point)


type alias Size =
    { height : Int
    , width : Int
    }


type alias GridRectangle =
    { bottomRow : Int
    , topRow : Int
    , leftColumn : Int
    , rightColumn : Int
    }


type alias Sizer a =
    a -> Size


type alias Grid a =
    { sizer : Sizer a
    , width : Int
    , height : Int
    , items : List ( a, GridRectangle )
    , edges : Edges
    }


appendAll : Grid a -> List a -> Grid a
appendAll g items =
    (foldl append g items)


forType : (a -> Size) -> Size -> Grid a
forType sizer gridSize =
    { sizer = sizer
    , width = gridSize.width
    , height = gridSize.height
    , items = []
    , edges = single { x = 1, y = 1 }
    }


fromBasePoint : Point -> Size -> GridRectangle
fromBasePoint point { width, height } =
    { leftColumn = point.x
    , rightColumn = point.x + width
    , topRow = point.y
    , bottomRow = point.y + height
    }


fits : Grid a -> GridRectangle -> Bool
fits grid rect =
    grid.height > rect.bottomRow


findPositionForItem : Grid a -> Size -> Maybe GridRectangle
findPositionForItem g s =
    (toPointList g.edges) |> List.map (\p -> fromBasePoint p s) |> List.filter (fits g) |> List.head


endOfCurrentColumn : GridRectangle -> Point
endOfCurrentColumn r =
    { x = r.leftColumn, y = r.bottomRow }


startOfNextColumn : GridRectangle -> Point
startOfNextColumn r =
    { x = r.rightColumn, y = 1 }


append : a -> Grid a -> Grid a
append frame grid =
    findPositionForItem grid (grid.sizer frame)
        |> Maybe.map
            (\newRect ->
                { grid
                    | items = grid.items ++ [ ( frame, newRect ) ]
                    , edges =
                        grid.edges
                            |> insert (endOfCurrentColumn newRect)
                            |> insert (startOfNextColumn newRect)
                }
            )
        |> withDefault grid
