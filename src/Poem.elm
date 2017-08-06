module Poem exposing (poem, Poem)

import Model exposing (GridContent(Content), GridItem, Model)


type alias Poem =
    { title : String
    , subtitle : String
    , lines : List String
    }


poem : Model -> Poem
poem model =
    { title = model.collection.title
    , subtitle = model.collection.dates
    , lines = model.movies |> sortByDistanceFromOrigin |> List.filterMap title
    }


sortByDistanceFromOrigin : List GridItem -> List GridItem
sortByDistanceFromOrigin =
    List.sortBy (\i -> (sqrt ((toFloat i.top) ^ 2) + ((toFloat i.left) ^ 2)))


title : GridItem -> Maybe String
title gridItem =
    case gridItem.content of
        Content _ _ movie _ ->
            Just movie.description

        _ ->
            Nothing
