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
    | ChangeMode Bool
    | Rotate Orientation
    | ToggleMenu Bool


toggleVideoPicker : Bool -> GridItem -> GridItem
toggleVideoPicker bool gridItem =
    case gridItem.content of
        Content o s m ms ->
            { gridItem | content = Content o s m { ms | videoPicker = bool } }

        _ ->
            gridItem


toggleMenu : Bool -> GridItem -> GridItem
toggleMenu bool gridItem =
    case gridItem.content of
        Content o s m ms ->
            { gridItem | content = Content o s m { ms | hoverMenu = bool } }

        _ ->
            gridItem


rotate : Orientation -> GridItem -> GridItem
rotate oldOrientation gridItem =
    case gridItem.content of
        Content o s m ms ->
            { gridItem
                | content =
                    Content
                        (Geometry.flipOrientation oldOrientation)
                        s
                        Nothing
                        defaultMenuState
            }

        _ ->
            gridItem


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
                setMovie movie >> (toggleMenu False) >> (toggleVideoPicker False)

            Resize scale ->
                resizeItem scale >> (toggleMenu False)

            ChangeMode mode ->
                toggleVideoPicker mode >> (toggleMenu (mode /= True))

            Rotate currentOrientation ->
                rotate currentOrientation

            ToggleMenu bool ->
                toggleMenu bool
        )
        index
        model


setMovie : Movie -> GridItem -> GridItem
setMovie newMovie gridItem =
    case gridItem.content of
        Content o s m ms ->
            { gridItem | content = Content o s (Just newMovie) ms }

        _ ->
            gridItem


resizeItem : Scale -> GridItem -> GridItem
resizeItem scale gridItem =
    case gridItem.content of
        Content o s m ms ->
            { gridItem | content = Content o scale m ms }

        _ ->
            gridItem


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


newItem : Position -> Model -> Model
newItem position model =
    { model
        | movies =
            model.movies
                ++ [ { top = position.y
                     , left = position.x
                     , content = Initial
                     }
                   ]
    }
