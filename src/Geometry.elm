module Geometry exposing (Point, Orientation(..), flipOrientation, Scale(..), Dimension, dimension)


type alias Point =
    { x : Int, y : Int }


type Orientation
    = Horizontal
    | Vertical


type Scale
    = Small
    | Medium
    | Large


type alias Dimension =
    { width : Int
    , height : Int
    }


flipOrientation : Orientation -> Orientation
flipOrientation orientation =
    if orientation == Horizontal then
        Vertical
    else
        Horizontal


dimension : Scale -> Orientation -> Dimension
dimension scale orientation =
    case ( scale, orientation ) of
        ( Small, Vertical ) ->
            { height = 340, width = 190 }

        ( Medium, Vertical ) ->
            { height = 520, width = 290 }

        ( Large, Vertical ) ->
            { height = 640, width = 370 }

        ( Small, Horizontal ) ->
            { height = 190, width = 340 }

        ( Medium, Horizontal ) ->
            { height = 290, width = 520 }

        ( Large, Horizontal ) ->
            { height = 370, width = 640 }
