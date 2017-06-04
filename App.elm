module App exposing (..)

import Html exposing (Html, div, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (foldl, head, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)


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


gridSize : Frame -> ( Int, Int )
gridSize { orientation, scale } =
    case orientation of
        Vertical ->
            case scale of
                Small ->
                    ( 3, 1 )

                Medium ->
                    ( 5, 2 )

                Large ->
                    ( 7, 3 )

        Horizontal ->
            case scale of
                Small ->
                    ( 1, 3 )

                Medium ->
                    ( 2, 5 )

                Large ->
                    ( 3, 7 )


type alias Position =
    { rows : ( Int, Int ), columns : ( Int, Int ) }


type Grid
    = Grid ( Int, Int, List ( Frame, Position ) )


grid =
    Grid ( 5, 7, [] )


bottomRightEdge : Position -> ( Int, Int )
bottomRightEdge { columns, rows } =
    let
        ( _, farColumn ) =
            columns

        ( _, farRow ) =
            rows
    in
        ( farColumn, farRow )


posForNext : Grid -> Frame -> Position
posForNext (Grid ( cols, rows, items )) frame =
    let
        ( x, y ) =
            Maybe.map second (last items)
                |> -- Maybe item == Maybe {
                   \m ->
                    Maybe.map bottomRightEdge m
                        |> \m -> Maybe.withDefault ( 0, 0 ) m

        ( xNeeded, yNeeded ) =
            gridSize frame
    in
        if y + yNeeded > rows then
            { rows = ( 0, yNeeded ), columns = ( x + 1, x + 1 + xNeeded ) }
        else
            { rows = ( y + 1, y + 1 + yNeeded ), columns = ( x, x + xNeeded ) }


gridAppend : Frame -> Grid -> Grid
gridAppend frame (Grid ( cols, rows, items )) =
    let
        newPos =
            posForNext grid frame

        newItems =
            ( frame, newPos ) :: items
    in
        Grid ( cols, rows, newItems )


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


movieView : ( Movie, ( Frame, Position ) ) -> Html Msg
movieView ( movie, ( frame, position ) ) =
    let
        ( col1, col2 ) =
            position.columns

        ( row1, row2 ) =
            position.rows
    in
        video
            [ (loop True)
            , (style
                [ ( "grid-row", (toString row1) ++ "/" ++ (toString row2) )
                , ( "grid-column", (toString col1) ++ "/" ++ (toString col2) )
                ]
              )
            , (src ("/public/" ++ movie.fileName))
            , (autoplay True)
            ]
            []


view : Model -> Html Msg
view model =
    let
        (Grid ( _, _, items )) =
            (foldl gridAppend grid model.layout.frames)
    in
        div
            [ (style
                [ ( "display", "grid" )
                , ( "grid-gap", "10px" )
                , ( "max-width", "90vw" )
                , ( "grid-auto-rows", "minmax(100px, auto)" )
                ]
              )
            ]
            (map movieView (zip model.layout.movies items))
