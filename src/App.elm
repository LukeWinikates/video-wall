module App exposing (..)

import Html exposing (Html, div, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (foldl, head, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Grid exposing (..)


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


type alias Frame =
    { orientation : Orientation, scale : Scale }


frameSize : Frame -> Size
frameSize { orientation, scale } =
    case ( orientation, scale ) of
        ( Vertical, Small ) ->
            { width = 1, height = 3 }

        ( Vertical, Medium ) ->
            { width = 2, height = 5 }

        ( Vertical, Large ) ->
            { width = 3, height = 7 }

        ( Horizontal, Small ) ->
            { width = 3, height = 1 }

        ( Horizontal, Medium ) ->
            { width = 5, height = 2 }

        ( Horizontal, Large ) ->
            { width = 7, height = 3 }


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



-- TODO: should the styles generated from the gridrectangle be encapsulated somehow?


movieView : ( Movie, ( Frame, GridRectangle ) ) -> Html Msg
movieView ( movie, ( frame, gridRectangle ) ) =
    div
        [ (style
            [ ( "grid-row", (toString gridRectangle.topRow) ++ "/" ++ (toString gridRectangle.bottomRow) )
            , ( "grid-column", (toString gridRectangle.leftColumn) ++ "/" ++ (toString gridRectangle.rightColumn) )
            , ( "background-color", "#ccc" )
            , ( "padding", "5px" )
            ]
          )
        ]
        [ video
            [ (loop True)
            , (style
                []
              )
            , (src ("/public/" ++ movie.fileName))
            , (autoplay True)
            ]
            []
        ]


makeGrid =
    Grid.forType frameSize



-- TODO: 24 9 is a pretty random gridsize.
-- TODO: make movies swappable
-- TODO: more layouts
-- TODO: layouts that allow stacking of wider elements and can fill in the space properly... going to be hard


view : Model -> Html Msg
view model =
    let
        grid =
            appendAll (makeGrid { width = 24, height = 9 }) model.layout.frames
    in
        div
            [ (style
                [ ( "display", "grid" )
                , ( "grid-gap", "10px" )
                , ( "max-width", "90vw" )
                , ( "justify-items", "center" )
                , ( "align-items", "center" )
                ]
              )
            ]
            (map movieView (zip model.layout.movies grid.items))
