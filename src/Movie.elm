module Movie exposing (Movie, byOrientation, findById)

import Geometry exposing (..)
import List

type alias Movie =
    { fileName : String
    , orientation : Orientation
    , description : String
    }


byOrientation : Orientation -> List Movie
byOrientation orientation =
    List.filter (\m -> (m.orientation == orientation)) movies |> Debug.log "movie"


movies : List Movie
movies =
    [ { fileName = "IMG_6212.m4v", orientation = Vertical, description = "Narrow angle through trees" }
    , { fileName = "IMG_6216.m4v", orientation = Horizontal, description = "Through thick trees" }
    , { fileName = "IMG_6230.m4v", orientation = Horizontal, description = "Green water with log in foreground" }
    , { fileName = "IMG_6213.m4v", orientation = Vertical, description = "Long distance across open river" }
    , { fileName = "IMG_6214.m4v", orientation = Vertical, description = "Upriver through trees" }
    , { fileName = "IMG_6219.m4v", orientation = Vertical, description = "Upriver from on top of log" }
    , { fileName = "IMG_6227.m4v", orientation = Horizontal, description = "Looking down into jade water" }
    , { fileName = "IMG_6231.m4v", orientation = Vertical, description = "Just woods" }
    , { fileName = "IMG_6244.m4v", orientation = Vertical, description = "Looking down into water channel" }
    , { fileName = "IMG_6250.m4v", orientation = Vertical, description = "Narrow view between two trees" }
    , { fileName = "IMG_6256.m4v", orientation = Vertical, description = "Heavy waterfall in background" }
    , { fileName = "IMG_6258.m4v", orientation = Horizontal, description = "Waterfall obscured by plants" }
    , { fileName = "IMG_6259.m4v", orientation = Vertical, description = "Barely visible waterfall" }
    , { fileName = "IMG_6260.m4v", orientation = Vertical, description = "Clearest closest waterfall view" }
    , { fileName = "IMG_6263.m4v", orientation = Horizontal, description = "Water flowing under log" }
    , { fileName = "IMG_6270.m4v", orientation = Vertical, description = "View downriver from sitting on log" }
    ]


findById : String -> Maybe Movie
findById id =
    movies |> List.filter (\m -> m.fileName == "IMG_" ++ id ++ ".m4v") |> List.head

