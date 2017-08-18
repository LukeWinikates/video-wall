module Model exposing (..)

import Dom.Dragging as Dragging
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
    = Content Orientation Scale Movie MenuState


type alias Model =
    { movies : List GridItem
    , collection : MovieCollection
    , dragging : Maybe (Dragging.Drag Int)
    , trayMode : TrayMode
    }


type TrayMode
    = Expanded TrayContent
    | Collapsed


type TrayContent
    = ShowingPoem
    | MoviePicker


empty : Model
empty =
    { movies = []
    , collection = Movie.fallbackCollection
    , dragging = Nothing
    , trayMode = Collapsed
    }


hydrate : MovieCollection -> ItemDescription -> Maybe GridItem
hydrate collection definition =
    Movie.findById collection definition.movieId
        |> Maybe.map
            (\movie ->
                { top = definition.top
                , left = definition.left
                , content =
                    Content
                        definition.orientation
                        definition.scale
                        movie
                        defaultMenuState
                }
            )


defaultMenuState : MenuState
defaultMenuState =
    { videoPicker = False
    , hoverMenu = False
    }


gridItemsFromCommaSeparatedList : MovieCollection -> String -> List GridItem
gridItemsFromCommaSeparatedList collection movieId =
    movieId |> String.split "," |> List.filterMap (Model.Parser.parseItem >> resultToMaybe) |> List.filterMap (hydrate collection)
