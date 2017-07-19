module Geometry exposing (Point, Orientation(..), flipOrientation)


type alias Point =
    { x : Int, y : Int }


type Orientation
    = Horizontal
    | Vertical


flipOrientation : Orientation -> Orientation
flipOrientation orientation =
    if orientation == Horizontal then
        Vertical
    else
        Horizontal
