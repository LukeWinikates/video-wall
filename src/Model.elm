module Model exposing (..)

import Dragging
import Movie exposing (..)
import Movie exposing (..)
import Geometry exposing (..)
import Mouse
import Model.Parser exposing (ItemDescription)
import Primitives exposing (resultToMaybe)


type alias GridItem =
    { top : Int
    , left : Int
    , content : GridContent
    }


type alias MenuState =
    { videoPicker : Bool
    , hoverMenu : Bool
    }


type GridContent
    = Initial (Maybe ( Orientation, Scale ))
    | Picking Orientation Scale
    | Content Orientation Scale Movie MenuState


type alias Model =
    { movies : List GridItem
    , collection : String
    , collectionMovies : List Movie
    , dragging : Maybe (Dragging.Drag Int)
    , trayMode : TrayMode
    }


type TrayMode
    = Expanded
    | Collapsed


empty : Model
empty =
    { movies = []
    , collectionMovies = []
    , collection = ""
    , dragging = Nothing
    , trayMode = Collapsed
    }


hydrate : String -> ItemDescription -> GridItem
hydrate collection definition =
    { top = definition.top
    , left = definition.left
    , content =
        (Movie.findById
            (Movie.fromCollection collection)
            definition.movieId
        )
            |> Maybe.map
                (\m ->
                    Content
                        definition.orientation
                        definition.scale
                        m
                        defaultMenuState
                )
            |> Maybe.withDefault (Picking definition.orientation definition.scale)
    }


defaultMenuState : MenuState
defaultMenuState =
    { videoPicker = False
    , hoverMenu = False
    }


gridMoviesFromUrlString : String -> String -> List GridItem
gridMoviesFromUrlString collectionName movieId =
    movieId |> String.split "," |> List.filterMap (Model.Parser.parseItem >> resultToMaybe) |> List.map (hydrate collectionName)
