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


showAllMovies : Model -> Model
showAllMovies model =
    { model
        | movies = (map (\f -> { f | mode = Showing }) model.movies)
    }


changeMovieAtIndex : (GridMovie -> GridMovie) -> Model -> Int -> Model
changeMovieAtIndex f model index =
    { model
        | movies =
            model.movies
                |> List.Extra.updateAt index f
                |> Maybe.withDefault model.movies
    }


swapMovie : Model -> Int -> Movie -> Model
swapMovie model index newMovie =
    changeMovieAtIndex (\m -> { m | movie = Just newMovie }) model index
        |> showAllMovies


changeMode : Model -> VideoMode -> Int -> Model
changeMode model mode index =
    changeMovieAtIndex (\frame -> { frame | mode = mode }) model index


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
resize scale index model =
    changeMovieAtIndex (resizeMovie scale) model index


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
            (changeMovieAtIndex
                (\gm ->
                    { gm
                        | top = position.y + drag.current.y - drag.start.y
                        , left = position.x + drag.current.x - drag.start.x
                    }
                )
                model
                index
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
