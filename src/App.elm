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

-- todo: Frame, Position, and Grid : are any of these redundant? Can they be eliminated?
-- todo: this seems too verbose. Can it be refactored?
-- todo: are frames relative to the Grid size -- the grid feels like a data structure that should be factored out into its own space, but that's been hard to do

gridSize : Frame -> ( Int, Int )
gridSize { orientation, scale } =
    case orientation of
        Vertical ->
            case scale of
                Small ->
                    ( 1, 3 )

                Medium ->
                    ( 2, 5 )

                Large ->
                    ( 3, 7 )

        Horizontal ->
            case scale of
                Small ->
                    ( 3, 1 )

                Medium ->
                    ( 5, 2 )

                Large ->
                    ( 7, 3 )


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


movieView : ( Movie, ( Frame, Position ) ) -> Html Msg
movieView ( movie, ( frame, position ) ) =
    let
        ( col1, col2 ) =
            position.columns

        ( row1, row2 ) =
            position.rows
    in
        div
            [ (style
                [ ( "grid-row", (toString row1) ++ "/" ++ (toString row2) )
                , ( "grid-column", (toString col1) ++ "/" ++ (toString col2) )
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


view : Model -> Html Msg
view model =
    let
        (Grid (_, _, _, items )) =
            (foldl append (Grid (gridSize, 12, 9, [] )) model.layout.frames)
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
            (map movieView (zip model.layout.movies items))
