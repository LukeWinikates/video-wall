module Model exposing (..)

import Dom.Dragging as Dragging
import Movie exposing (..)
import Movie exposing (..)
import Geometry exposing (..)
import Mouse exposing (Position)
import Model.Parser exposing (ItemDescription)
import Primitives exposing (resultToMaybe)
import Time exposing (Time)


type alias GridItem =
    { top : Int
    , left : Int
    , orientation : Orientation
    , scale : Scale
    , movie : Movie
    , menuState : MenuState
    }


type alias MenuState =
    { hoverMenu : Bool
    }


type alias Model =
    { movies : List GridItem
    , collection : MovieCollection
    , dragging : Maybe (Dragging.Drag Int)
    , trayMode : TrayMode
    , lastInteractionTime : Time
    , lastTick : Time
    }


type TrayMode
    = Expanded TrayContent
    | Collapsed


type alias PickerState =
    { highlighted : Maybe Movie, position : Position }


type TrayContent
    = ShowingPoem
    | MoviePicker PickerState


empty : Model
empty =
    { movies = []
    , collection = Movie.fallbackCollection
    , dragging = Nothing
    , trayMode = Collapsed
    , lastInteractionTime = 0
    , lastTick = 0
    }


userHasInteractedRecently : Model -> Bool
userHasInteractedRecently model =
    model.lastInteractionTime
        > (model.lastTick - 1500)


default : Model
default =
    let
        collection =
            Movie.fromCollectionId "sanjuan" |> Maybe.withDefault Movie.fallbackCollection
    in
        { empty
            | collection = collection
            , movies =
                [ "49-16-V-S-6444"
                , "75-257-H-M-6443"
                , "428-22-H-L-6447"
                , "448-701-V-S-6450"
                , "430-935-H-L-6468"
                , "77-838-H-M-6469"
                , "50-1376-V-S-6451"
                ]
                    |> String.join ","
                    |> gridItemsFromCommaSeparatedList collection
        }


hydrate : MovieCollection -> ItemDescription -> Maybe GridItem
hydrate collection definition =
    Movie.findById collection definition.movieId
        |> Maybe.map
            (\movie ->
                { top = definition.top
                , left = definition.left
                , orientation = definition.orientation
                , scale = definition.scale
                , movie = movie
                , menuState = defaultMenuState
                }
            )


defaultMenuState : MenuState
defaultMenuState =
    { hoverMenu = False
    }


gridItemsFromCommaSeparatedList : MovieCollection -> String -> List GridItem
gridItemsFromCommaSeparatedList collection movieId =
    movieId |> String.split "," |> List.filterMap (Model.Parser.parseItem >> resultToMaybe) |> List.filterMap (hydrate collection)


dimensionsForContent : GridItem -> Dimension
dimensionsForContent item =
    dimension item.scale item.orientation
