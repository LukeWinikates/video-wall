module Model.Mutate exposing (..)

import Dragging exposing (Drag)
import Geometry exposing (Orientation(Horizontal, Vertical))
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


type alias Dimension =
    { width : Int
    , height : Int
    }


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
        |> resizeMovie Large


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


changePosition : Position -> GridMovie -> GridMovie
changePosition offset gridMovie =
    { gridMovie
        | top = gridMovie.top + offset.y
        , left = gridMovie.left + offset.x
    }


drag : Maybe (Drag Int) -> Model -> Model
drag maybeDrag model =
    { model | dragging = maybeDrag }


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
                     , menu = False
                     }
                   ]
    }
