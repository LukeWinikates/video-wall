module Edges exposing (Edges, empty, insert, toPointList, single)

import Dict exposing (Dict)
import Point exposing (Point)


type alias Edges =
    Dict Int Int


empty =
    Dict.empty


insert : Point -> Edges -> Edges
insert point edges =
    Dict.insert point.x point.y edges


toPointList : Edges -> List Point
toPointList e =
    Dict.toList e |> List.map (\( x, y ) -> { x = x, y = y })


single point =
    insert point empty
