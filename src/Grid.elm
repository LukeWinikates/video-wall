module Grid exposing (append, GridRectangle, appendAll, forType)

import List exposing (foldl)
import List.Extra exposing (zip, last)
import Tuple exposing (second)


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
    , edges : List Point
    }


appendAll : Grid a -> List a -> Grid a
appendAll g items =
    (foldl append g items)


forType : (a -> Size) -> Int -> Int -> Grid a
forType sizer x y =
    { sizer = sizer, width = x, height = y, items = [], edges = [] }


fromBasePoint : Point -> Size -> GridRectangle
fromBasePoint point ( width, height ) =
    { leftColumn = point.x
    , rightColumn = point.x + width
    , topRow = point.y
    , bottomRow = point.y + height
    }


sizeHeight =
    second



-- TODO: the grid stores not just a list of items, but a list of the leading edges of columns
-- TODO: so rather than consider merely the latest column, we can consider each column in that data structure in turn


posForNext : Grid a -> a -> GridRectangle
posForNext g sizable =
    let
        ( widthNeeded, heightNeeded ) =
            g.sizer sizable

        fbp =
            \p -> fromBasePoint p ( widthNeeded, heightNeeded )
    in
        case last g.items of
            Nothing ->
                fbp { x = 1, y = 1 }

            Just ( _, lastItem ) ->
                if lastItem.bottomRow + heightNeeded > g.height then
                    fbp { x = lastItem.rightColumn, y = 1 }
                else
                    fbp { x = lastItem.leftColumn, y = lastItem.bottomRow }


append : a -> Grid a -> Grid a
append frame grid =
    { grid
        | items = grid.items ++ [ ( frame, posForNext grid frame ) ]
    }
