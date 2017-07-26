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


type alias AbstractDimension =
    { longSide : Int
    , shortSide : Int
    }


orientedDimension : Orientation -> AbstractDimension -> Dimension
orientedDimension orientation dim =
    case orientation of
        Horizontal ->
            { height = dim.shortSide, width = dim.longSide }

        Vertical ->
            { height = dim.longSide, width = dim.shortSide }


abstractDimension : Scale -> AbstractDimension
abstractDimension scale =
    case scale of
        Small ->
            { longSide = 340, shortSide = 200 }

        Medium ->
            { longSide = 500, shortSide = 290 }

        Large ->
            { longSide = 640, shortSide = 370 }


dimension : Scale -> Orientation -> Dimension
dimension scale orientation =
    abstractDimension scale |> orientedDimension orientation
