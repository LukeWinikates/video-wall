module Model.Serialize exposing (..)

import Geometry exposing (Orientation(Horizontal, Vertical), Scale(Large, Medium, Small))
import Model exposing (GridContent(Content), GridItem, Model)


frameToString : GridItem -> Maybe String
frameToString { content, top, left } =
    case content of
        Content orientation scale movie _ ->
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
                |> Just

        _ ->
            Nothing


framesUrlString : List GridItem -> String
framesUrlString frames =
    frames |> List.filterMap frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "#/" ++ model.collection.id ++ "/" ++ (framesUrlString model.movies)
