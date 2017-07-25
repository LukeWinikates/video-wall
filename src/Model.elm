module Model exposing (..)

import Dragging
import Movie exposing (..)
import Movie exposing (..)
import Geometry exposing (..)
import Mouse
import Movie.Parser exposing (MovieDefinition)
import Primitives exposing (resultToMaybe)


type alias GridMovie =
    { orientation : Orientation
    , top : Int
    , height : Int
    , left : Int
    , width : Int
    , mode : VideoMode
    , movie : Maybe Movie
    , menu : Bool
    }


type alias Model =
    { movies : List GridMovie
    , collection : String
    , collectionMovies : List Movie
    , dragging : Maybe (Dragging.Drag Int)
    }


type Scale
    = Small
    | Medium
    | Large


type VideoMode
    = Menu
    | Showing


empty : Model
empty =
    { movies = []
    , collectionMovies = []
    , collection = ""
    , dragging = Nothing
    }


hydrate : String -> MovieDefinition -> GridMovie
hydrate collection definition =
    { orientation = definition.orientation
    , top = definition.top
    , height = definition.height
    , left = definition.left
    , width = definition.width
    , movie = Movie.findById (Movie.fromCollection collection) definition.movieId
    , mode = Showing
    , menu = False
    }


gridMoviesFromUrlString : String -> String -> List GridMovie
gridMoviesFromUrlString collectionName movieId =
    movieId |> String.split "," |> List.filterMap (Movie.parseString >> resultToMaybe) |> List.map (hydrate collectionName)


frameToString : GridMovie -> String
frameToString { orientation, top, left, height, width, movie } =
    [ (case orientation of
        Horizontal ->
            "H"

        Vertical ->
            "V"
      )
    , toString top
    , toString left
    , toString height
    , toString width
    , Maybe.map .id movie |> Maybe.withDefault "N"
    ]
        |> String.join "-"


framesUrlString : List GridMovie -> String
framesUrlString frames =
    frames |> List.map frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "?movies=" ++ (framesUrlString model.movies) ++ "&collection=" ++ model.collection
