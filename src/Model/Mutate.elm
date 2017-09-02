module Model.Mutate exposing (..)

import Dom.Dragging as Dragging exposing (Drag)
import Geometry exposing (Orientation(Horizontal, Vertical), Scale(Large, Medium, Small))
import Model exposing (..)
import List.Extra
import Movie exposing (Movie)
import List exposing (map)
import Mouse exposing (Position)


type Mutation
    = Swap Movie
    | Resize Scale
    | ShowHoverMenu Bool
    | Rotate Orientation


toggleHoverMenu : Bool -> GridItem -> GridItem
toggleHoverMenu bool gridItem =
    let
        ms =
            gridItem.menuState
    in
        { gridItem | menuState = { ms | hoverMenu = bool } }


clearMenus : GridItem -> GridItem
clearMenus gridItem =
    { gridItem | menuState = defaultMenuState }


rotate : Orientation -> GridItem -> GridItem
rotate oldOrientation gridItem =
    { gridItem
        | orientation =
            (Geometry.flipOrientation oldOrientation)
    }


applyAll : (GridItem -> GridItem) -> Model -> Model
applyAll f m =
    { m | movies = List.map f m.movies }


remove : Int -> Model -> Model
remove index model =
    { model | movies = List.Extra.removeAt index model.movies }


applyAtIndex : (GridItem -> GridItem) -> Int -> Model -> Model
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
                setMovie movie >> (toggleHoverMenu True)

            Resize scale ->
                resizeItem scale >> (toggleHoverMenu False)

            Rotate currentOrientation ->
                rotate currentOrientation

            ShowHoverMenu bool ->
                toggleHoverMenu bool
        )
        index
        model


setMovie : Movie -> GridItem -> GridItem
setMovie newMovie gridItem =
    { gridItem | movie = newMovie }


resizeItem : Scale -> GridItem -> GridItem
resizeItem scale gridItem =
    { gridItem | scale = scale }


resize : Scale -> Int -> Model -> Model
resize scale index =
    applyAtIndex (resizeItem scale) index


changePosition : Position -> GridItem -> GridItem
changePosition offset gridMovie =
    { gridMovie
        | top = gridMovie.top + offset.y
        , left = gridMovie.left + offset.x
    }


drag : Maybe (Drag Int) -> Model -> Model
drag maybeDrag model =
    { model | dragging = maybeDrag }


hideTray : Model -> Model
hideTray model =
    { model | trayMode = Collapsed }


newItem : Movie -> Position -> Model -> Model
newItem movie position model =
    { model
        | movies =
            model.movies
                ++ [ { top = position.y
                     , left = position.x
                     , orientation = movie.orientation
                     , scale = Medium
                     , movie = movie
                     , menuState = defaultMenuState
                     }
                   ]
    }
