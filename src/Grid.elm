module Grid exposing (sizer, append, Grid(..), Position, items, appendAll)

import List exposing (foldl)
import List.Extra exposing (zip, last)
import Tuple exposing (second)

type alias Size = (Int, Int)

type alias Position =
    { rows : ( Int, Int ), columns : ( Int, Int ) }

type Grid a
    = Grid (a -> Size, Int, Int, List ( a, Position ) )

sizer : Grid a -> (a -> Size)
sizer (Grid (f, _,_,_)) = f

appendAll : Grid a -> List a -> Grid a
appendAll g items= (foldl append (Grid (sizer g, 12, 9, [] )) items)

items : Grid a -> List (a, Position)
items (Grid(_,_,_,i)) = i

-- TODO: the grid stores not just a list of items, but a list of the leading edges of columns
-- TODO: so rather than consider merely the latest column, we can consider each column in that data structure in turn
posForNext : Grid a -> a -> Position
posForNext (Grid (sizer, gridWidth, gridHeight, items )) frame =
    let
        ( widthNeeded, heightNeeded ) =
            sizer frame
    in
        case last items of
            Nothing ->
                { rows = ( 1, 1 + heightNeeded ), columns = ( 1, 1 + widthNeeded ) }

            Just ( _, lastItem ) ->
                if (lastItem.rows |> second) + heightNeeded > gridHeight then
                    { rows = ( 1, 1 + heightNeeded ), columns = ( lastItem.columns |> second, (lastItem.columns |> second) + widthNeeded ) }
                else
                    { rows = ( lastItem.rows |> second, (lastItem.rows |> second) + heightNeeded ), columns = lastItem.columns }


append : a -> Grid a -> Grid a
append frame grid =
    let
        (Grid (s, cols, rows, items )) =
            grid
    in
        Grid (s, cols, rows, List.append items [ ( frame, posForNext grid frame ) ] )