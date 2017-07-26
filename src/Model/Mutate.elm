module Model.Mutate exposing (..)

import Dragging exposing (Drag)
import Geometry exposing (Orientation(Horizontal, Vertical), Scale(Large, Medium, Small))
import Model exposing (..)
import List.Extra
import Movie exposing (Movie)
import List exposing (map)
import Mouse exposing (Position)


type Mutation
    = Swap Movie
    | Resize Scale
    | ChangeMode VideoMode
    | Rotate Orientation
    | ToggleMenu Bool


changeMode : VideoMode -> GridMovie -> GridMovie
changeMode mode gridMovie =
    { gridMovie | mode = mode }


toggleMenu : Bool -> GridMovie -> GridMovie
toggleMenu bool gridMovie =
    { gridMovie | menu = bool }


rotate : Orientation -> GridMovie -> GridMovie
rotate oldOrientation gridMovie =
    { gridMovie
        | orientation = Geometry.flipOrientation oldOrientation
        , mode = Menu
        , movie = Nothing
    }


applyAll : (GridMovie -> GridMovie) -> Model -> Model
applyAll f m =
    { m | movies = List.map f m.movies }


remove : Int -> Model -> Model
remove index model =
    { model | movies = List.Extra.removeAt index model.movies }


applyAtIndex : (GridMovie -> GridMovie) -> Int -> Model -> Model
applyAtIndex f index model =
    { model
        | movies =
            model.movies
                |> List.Extra.updateAt index f
                |> Maybe.withDefault model.movies
    }


applyMutationAtIndex : Mutation -> Int -> Model -> Model
applyMutationAtIndex mutation index model =
    applyAtIndex
        (case mutation of
            Swap movie ->
                setMovie movie >> (toggleMenu False) >> (changeMode Showing)

            Resize scale ->
                resizeMovie scale >> (toggleMenu False)

            ChangeMode mode ->
                changeMode mode >> (toggleMenu (mode /= Showing))

            Rotate currentOrientation ->
                rotate currentOrientation

            ToggleMenu bool ->
                toggleMenu bool
        )
        index
        model


setMovie : Movie -> GridMovie -> GridMovie
setMovie newMovie gridMovie =
    { gridMovie | movie = Just newMovie }


resizeMovie : Scale -> GridMovie -> GridMovie
resizeMovie scale gridMovie =
    { gridMovie | scale = scale }


resize : Scale -> Int -> Model -> Model
resize scale index =
    applyAtIndex (resizeMovie scale) index


changePosition : Position -> GridMovie -> GridMovie
changePosition offset gridMovie =
    { gridMovie
        | top = gridMovie.top + offset.y
        , left = gridMovie.left + offset.x
    }


drag : Maybe (Drag Int) -> Model -> Model
drag maybeDrag model =
    { model | dragging = maybeDrag }


newMovie : Orientation -> Scale -> Position -> Model -> Model
newMovie orientation scale position model =
    { model
        | movies =
            model.movies
                ++ [ { orientation = orientation
                     , top = position.y
                     , left = position.x
                     , scale = scale
                     , movie = Nothing
                     , mode = Menu
                     , menu = False
                     }
                   ]
    }
