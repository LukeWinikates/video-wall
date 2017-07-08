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
        , ( "lawizwiz"
          , [ { id = "6360", orientation = Horizontal, description = "Fast water through logs" }
            , { id = "6361", orientation = Vertical, description = "Rapids with log in foreground" }
            , { id = "6363", orientation = Vertical, description = "Log's eye view" }
            , { id = "6364", orientation = Vertical, description = "Rocks in shallow water" }
            , { id = "6369", orientation = Horizontal, description = "Logs on logs obscuring river" }
            , { id = "6370", orientation = Vertical, description = "Just waterfall" }
            , { id = "6371", orientation = Horizontal, description = "Water rushing left to right" }
            , { id = "6372", orientation = Horizontal, description = "The rock's whole life is being wet" }
            , { id = "6373", orientation = Vertical, description = "The rock's whole life is being wet" }
            , { id = "6387", orientation = Horizontal, description = "Ripples in a quiet spot" }
            , { id = "6394", orientation = Vertical, description = "The stream turns here" }
            ]
          )
        , ( "sanjuan"
          , [ { id = "6443", orientation = Horizontal, description = "Big wide bay" }
            , { id = "6444", orientation = Vertical, description = "Sun in tree" }
            , { id = "6447", orientation = Horizontal, description = "Island with tree in foreground" }
            , { id = "6450", orientation = Vertical, description = "Looking down onto rocks" }
            , { id = "6451", orientation = Vertical, description = "Island with rocks way out there" }
            , { id = "6452", orientation = Vertical, description = "Mostly underwater rock" }
            , { id = "6453", orientation = Vertical, description = "Soft waves on rocks" }
            , { id = "6456", orientation = Vertical, description = "Red light in the trees" }
            , { id = "6467", orientation = Vertical, description = "Made it to the beach" }
            , { id = "6468", orientation = Horizontal, description = "Kayaks in the offing" }
            , { id = "6469", orientation = Horizontal, description = "Pebbles getting smoother" }
            , { id = "6473", orientation = Vertical, description = "Yellow flower blue water" }
            , { id = "6474", orientation = Vertical, description = "Flower and the beach" }
            , { id = "6475", orientation = Vertical, description = "Rocky spit" }
            , { id = "6476", orientation = Vertical, description = "Looking at the water from the shade" }
            , { id = "6478", orientation = Vertical, description = "Looking at the water from the shade 2" }
            , { id = "6485", orientation = Vertical, description = "Flower and very clear water" }
            , { id = "6486", orientation = Vertical, description = "Flower and very clear water 2" }
            , { id = "6487", orientation = Horizontal, description = "Just the water really" }
            , { id = "6488", orientation = Horizontal, description = "Cliff, and sounds" }
            , { id = "6492", orientation = Vertical, description = "Probably the gentlest waves there are" }
            , { id = "6493", orientation = Horizontal, description = "Probably the gentlest waves there are 2" }
            , { id = "6498", orientation = Vertical, description = "Crook of the tree" }
            , { id = "6504", orientation = Vertical, description = "View from the lime kiln" }
            , { id = "6505", orientation = Horizontal, description = "Looking at the water from the shade 3" }
            , { id = "6506", orientation = Vertical, description = "Branches/Horizon" }
            , { id = "6507", orientation = Horizontal, description = "Still hours to nightfall" }
            , { id = "6531", orientation = Vertical, description = "Are ferries slow or fast?" }
            , { id = "6532", orientation = Horizontal, description = "View from the car" }
            , { id = "6533", orientation = Horizontal, description = "Island view from the ferry" }
            ]
          )
        ]


findById : List Movie -> String -> Maybe Movie
findById movies id =
    movies |> List.filter (\m -> m.id == id) |> List.head
