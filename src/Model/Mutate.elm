module Model.Mutate exposing (..)

import Geometry exposing (Orientation(Horizontal, Vertical))
import Model exposing (..)
import List.Extra
import Movie exposing (Movie)
import List exposing (map)
import Mouse exposing (Position)


-- TODO: instead of a bunch of helpers of pattern changeMovieAtIndex + movie update function,
-- just pull expose all the movie update functions at the top level.
-- or could define each thing as a mapping from a command to a mutation for an index and a mutation for all movies
-- { updateMovie = swap idx movie, updateAll = changeMode showing }


changeMode : VideoMode -> GridMovie -> GridMovie
changeMode mode gridMovie =
    { gridMovie | mode = mode }


applyAll : (GridMovie -> GridMovie) -> Model -> Model
applyAll f m =
    { m | movies = List.map f m.movies }


applyAtIndex : (GridMovie -> GridMovie) -> Int -> Model -> Model
applyAtIndex f index model =
    { model
        | movies =
            model.movies
                |> List.Extra.updateAt index f
                |> Maybe.withDefault model.movies
    }


swapMovie : Model -> Int -> Movie -> Model
swapMovie model index newMovie =
    applyAtIndex (\m -> { m | movie = Just newMovie }) index model
        |> applyAll (changeMode Showing)


type alias Dimension =
    { width : Int
    , height : Int
    }


dimension : Scale -> Orientation -> Dimension
dimension scale orientation =
    case ( scale, orientation ) of
        ( Small, Vertical ) ->
            { height = 340, width = 190 }

        ( Medium, Vertical ) ->
            { height = 516, width = 290 }

        ( Large, Vertical ) ->
            { height = 640, width = 360 }

        ( Small, Horizontal ) ->
            { height = 190, width = 340 }

        ( Medium, Horizontal ) ->
            { height = 290, width = 516 }

        ( Large, Horizontal ) ->
            { height = 360, width = 640 }


resizeMovie : Scale -> GridMovie -> GridMovie
resizeMovie scale gridMovie =
    let
        newScale =
            dimension scale gridMovie.orientation
    in
        { gridMovie | width = newScale.width, height = newScale.height }


resize : Scale -> Int -> Model -> Model
resize scale index =
    applyAtIndex (resizeMovie scale) index


newMovie : Orientation -> Model -> Model
newMovie orientation model =
    { model
        | movies =
            model.movies
                ++ [ { orientation = orientation
                     , top = 500
                     , height = 1000
                     , left = 50
                     , width = 350
                     , movie = Nothing
                     , mode = Menu
                     }
                   ]
    }


dragMovie : Model -> Position -> Int -> Model
dragMovie model position index =
    (Maybe.map
        (\drag ->
            (applyAtIndex
                (\gm ->
                    { gm
                        | top = position.y + drag.current.y - drag.start.y
                        , left = position.x + drag.current.x - drag.start.x
                    }
                )
                index
                model
            )
        )
        model.dragging
    )
        |> Maybe.withDefault model


updateDrag : Model -> Position -> Model
updateDrag model position =
    (Maybe.map
        (\drag -> { model | dragging = Just { index = drag.index, start = drag.start, current = position } })
        model.dragging
    )
        |> Maybe.withDefault model


moveToNewDragPosition : Model -> Position -> Int -> Model
moveToNewDragPosition model position index =
    ((dragMovie model position index) |> (\m -> updateDrag m position))
