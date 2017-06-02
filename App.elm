module App exposing (..)

import Html exposing (Html, div, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (head, map, tail, take)
import List.Extra exposing (zip)


type Orientation
    = Horizontal
    | Vertical


type alias Movie =
    { fileName : String
    , orientation : Orientation
    }


movies : List Movie
movies =
    [ { fileName = "IMG_6212.m4v", orientation = Vertical }
    , { fileName = "IMG_6213.m4v", orientation = Vertical }
    , { fileName = "IMG_6214.m4v", orientation = Vertical }
    , { fileName = "IMG_6219.m4v", orientation = Vertical }
    , { fileName = "IMG_6227.m4v", orientation = Horizontal }
    , { fileName = "IMG_6230.m4v", orientation = Horizontal }
    , { fileName = "IMG_6231.m4v", orientation = Vertical }
    , { fileName = "IMG_6244.m4v", orientation = Vertical }
    , { fileName = "IMG_6250.m4v", orientation = Vertical }
    , { fileName = "IMG_6256.m4v", orientation = Vertical }
    , { fileName = "IMG_6258.m4v", orientation = Horizontal }
    , { fileName = "IMG_6259.m4v", orientation = Vertical }
    , { fileName = "IMG_6260.m4v", orientation = Vertical }
    , { fileName = "IMG_6263.m4v", orientation = Horizontal }
    , { fileName = "IMG_6270.m4v", orientation = Vertical }
    ]


type Scale
    = Small
    | Medium
    | Large


type alias Layout =
    { frames : List Frame, movies : List Movie }


frames =
    [ { orientation = Vertical, scale = Large }
    , { orientation = Horizontal, scale = Medium }
    , { orientation = Horizontal, scale = Medium }
    , { orientation = Vertical, scale = Large }
    ]



--layout = a list of frames
-- frame = an orientation


type alias Frame =
    { orientation : Orientation, scale : Scale }



-- size Vertical Small = 1,2
-- size Vertical Medium = 2,4
-- size Vertical Large = 4,8
-- size Horizontal Small = 2,1
-- size Horizontal Medium = 4,2
-- size Horizontal Large = 8,4


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { layout : Layout }


init : ( Model, Cmd Msg )
init =
    ( { layout =
            { frames = frames
            , movies =
                [ { fileName = "IMG_6250.m4v", orientation = Vertical }
                , { fileName = "IMG_6227.m4v", orientation = Horizontal }
                , { fileName = "IMG_6230.m4v", orientation = Horizontal }
                , { fileName = "IMG_6231.m4v", orientation = Vertical }
                ]
            }
      }
    , Cmd.none
    )


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    ( model, Cmd.none )


w : Frame -> String
w f =
    case f.orientation of
        Vertical ->
            "20%"

        Horizontal ->
            "30%"


movieView : ( Movie, Frame ) -> Html Msg
movieView ( movie, frame ) =
    video
        [ (loop True)
        , (style
            [ ( "width", w frame )
            ]
          )
        , (src ("/public/" ++ movie.fileName))
        , (autoplay True)
        ]
        []


view : Model -> Html Msg
view model =
    div
        []
        (map movieView (zip model.layout.movies model.layout.frames))
