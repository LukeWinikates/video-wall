module Movie exposing (Movie, byOrientation, findById, fileName, fromCollection)

import Dict exposing (Dict)
import Geometry exposing (..)
import List
import Maybe exposing (withDefault)


type alias Movie =
    { id : String
    , orientation : Orientation
    , description : String
    }


byOrientation : List Movie -> Orientation -> List Movie
byOrientation movies orientation =
    List.filter (\m -> (m.orientation == orientation)) movies |> Debug.log "movie"


fromCollection : String -> List Movie
fromCollection collection =
    Dict.get collection movies |> withDefault []


fileName : Movie -> String
fileName movie =
    "IMG_" ++ movie.id ++ ".m4v"


movies : Dict String (List Movie)
movies =
    Dict.fromList
        [ ( "ironcreek"
          , [ { id = "6212", orientation = Vertical, description = "Narrow angle through trees" }
            , { id = "6216", orientation = Horizontal, description = "Through thick trees" }
            , { id = "6230", orientation = Horizontal, description = "Green water with log in foreground" }
            , { id = "6213", orientation = Vertical, description = "Long distance across open river" }
            , { id = "6214", orientation = Vertical, description = "Upriver through trees" }
            , { id = "6219", orientation = Horizontal, description = "Upriver from on top of log" }
            , { id = "6227", orientation = Horizontal, description = "Looking down into jade water" }
            , { id = "6231", orientation = Vertical, description = "Just woods" }
            , { id = "6244", orientation = Vertical, description = "Looking down into water channel" }
            , { id = "6250", orientation = Vertical, description = "Narrow view between two trees" }
            , { id = "6256", orientation = Vertical, description = "Heavy waterfall in background" }
            , { id = "6258", orientation = Horizontal, description = "Waterfall obscured by plants" }
            , { id = "6259", orientation = Vertical, description = "Barely visible waterfall" }
            , { id = "6260", orientation = Vertical, description = "Clearest closest waterfall view" }
            , { id = "6263", orientation = Horizontal, description = "Water flowing under log" }
            , { id = "6270", orientation = Vertical, description = "View downriver from sitting on log" }
            ]
          )
        ]


findById : List Movie -> String -> Maybe Movie
findById movies id =
    movies |> List.filter (\m -> m.id == id) |> List.head
