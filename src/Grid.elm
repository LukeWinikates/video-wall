module Grid exposing (append, GridRectangle, appendAll, forType)

import List exposing (foldl)
import List.Extra exposing (zip, last)
import Tuple exposing (second)

type alias Size = (Int, Int)

type alias GridRectangle =
    { rows : ( Int, Int ), columns : ( Int, Int ) }

type alias Sizer a = (a -> Size)

type alias Grid a =
  { sizer : Sizer a,
   width : Int,
   height : Int,
   items: List (a, GridRectangle),
   edges: List (Int,Int)
  }


appendAll : Grid a -> List a -> Grid a
appendAll g items= (foldl append g items)

forType : (a -> Size) -> Int -> Int -> Grid a
forType sizer x y = { sizer = sizer, width = x, height = y, items = [], edges = [] }

-- TODO: the grid stores not just a list of items, but a list of the leading edges of columns
-- TODO: so rather than consider merely the latest column, we can consider each column in that data structure in turn
posForNext : Grid a -> a -> GridRectangle
posForNext g sizable =
    let
        ( widthNeeded, heightNeeded ) =
            g.sizer sizable
    in
        case last g.items of
            Nothing ->
                { rows = ( 1, 1 + heightNeeded ), columns = ( 1, 1 + widthNeeded ) }

            Just ( _, lastItem ) ->
                if (lastItem.rows |> second) + heightNeeded > g.height then
                    { rows = ( 1, 1 + heightNeeded ), columns = ( lastItem.columns |> second, (lastItem.columns |> second) + widthNeeded ) }
                else
                    { rows = ( lastItem.rows |> second, (lastItem.rows |> second) + heightNeeded ), columns = lastItem.columns }


append : a -> Grid a -> Grid a
append frame grid =
         { grid | items = List.append grid.items [ ( frame, posForNext grid frame ) ] }