module App.GridEdges exposing (..)

import Model exposing (GridItem, Model)


bottomEdge : Model -> Int
bottomEdge model =
    model.movies
        |> List.map itemBottomEdge
        |> List.maximum
        |> Maybe.withDefault 20


itemBottomEdge : GridItem -> Int
itemBottomEdge item =
    (Model.dimensionsForContent item.content)
        |> .height
        |> (+) item.top


rightEdge : Model -> Int
rightEdge model =
    model.movies
        |> List.map itemRightEdge
        |> List.maximum
        |> Maybe.withDefault 20


itemRightEdge : GridItem -> Int
itemRightEdge item =
    (Model.dimensionsForContent item.content)
        |> .width
        |> (+) item.left


leftEdge : Model -> Int
leftEdge model =
    model.movies
        |> List.map .left
        |> List.minimum
        |> Maybe.withDefault 20


topEdge : Model -> Int
topEdge model =
    model.movies
        |> List.map .top
        |> List.minimum
        |> Maybe.withDefault 20
