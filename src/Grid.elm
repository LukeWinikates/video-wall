module Grid exposing (append, GridRectangle, appendAll, forType)

import List exposing (foldl)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Maybe exposing (withDefault)
import Edges exposing (Edges, insert, single, toPointList)
import Point exposing (Point)


-- TODO: change this to a record so the destrcturing isn't as painful


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


forType : (a -> Size) -> Int -> Int -> Grid a
forType sizer x y =
    { sizer = sizer, width = x, height = y, items = [], edges = single { x = 1, y = 1 } }


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



-- TODO: this is kind of a bad name.


nextRootPos : Grid a -> Size -> Maybe GridRectangle
nextRootPos g s =
    (toPointList g.edges) |> List.map (\p -> fromBasePoint p s) |> List.filter (fits g) |> List.head



-- TODO: generating the two possible edges is a little funky - should it be its own function?


append : a -> Grid a -> Grid a
append frame grid =
    nextRootPos grid (grid.sizer frame)
        |> Maybe.map
            (\newRect ->
                { grid
                    | items = grid.items ++ [ ( frame, newRect ) ]
                    , edges =
                        grid.edges
                            |> insert { x = newRect.leftColumn, y = newRect.bottomRow }
                            |> insert { x = newRect.rightColumn, y = 1 }
                }
            )
        |> withDefault grid
