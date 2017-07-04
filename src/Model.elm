module Model exposing (..)

import Movie exposing (..)
import MovieParser exposing (..)
import Geometry exposing (..)
import Mouse
import Primitives exposing (resultToMaybe)


type alias GridMovie =
    { orientation : Orientation
    , top : Int
    , height : Int
    , left : Int
    , width : Int
    , mode : VideoMode
    , movie : Maybe Movie
    }


type alias Drag =
    { index : Int
    , start : Mouse.Position
    , current : Mouse.Position
    }


type alias Model =
    { movies : List GridMovie
    , dragging : Maybe Drag
    }


type Scale
    = Small
    | Medium
    | Large


type VideoMode
    = Menu
    | Showing
    | Buttons


hydrate : MovieDefinition -> GridMovie
hydrate definition =
    { orientation = definition.orientation
    , top = definition.top
    , height = definition.height
    , left = definition.left
    , width = definition.width
    , movie = Movie.findById definition.movieId
    , mode = Showing
    }


gridMoviesFromUrlString : String -> List GridMovie
gridMoviesFromUrlString =
    String.split "," >> List.filterMap (MovieParser.parseMovie >> resultToMaybe) >> List.map hydrate


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
    "?movies=" ++ (framesUrlString model.movies)
