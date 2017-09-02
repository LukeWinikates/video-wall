module Model.Serialize exposing (..)

import Geometry exposing (Orientation(Horizontal, Vertical), Scale(Large, Medium, Small))
import Model exposing (GridItem, Model)


frameToString : GridItem -> String
frameToString { top, left, orientation, scale, movie } =
    [ toString top
    , toString left
    , (case orientation of
        Horizontal ->
            "H"

        Vertical ->
            "V"
      )
    , (case scale of
        Small ->
            "S"

        Medium ->
            "M"

        Large ->
            "L"
      )
    , movie.id
    ]
        |> String.join "-"


framesUrlString : List GridItem -> String
framesUrlString frames =
    frames |> List.map frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "#/" ++ model.collection.id ++ "/" ++ (framesUrlString model.movies)
