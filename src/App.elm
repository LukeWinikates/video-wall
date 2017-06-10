module App exposing (..)

import Html exposing (Html, div, li, text, ul, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (foldl, head, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Grid exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, oneOf, parseHash, (<?>), stringParam, top)


type Route
    = LayoutsRoute (Maybe String) (Maybe String)


sizeFromString : Char -> Scale
sizeFromString str =
    case str of
        'S' ->
            Small

        'M' ->
            Medium

        _ ->
            Large


framesFromStrings : List Frame -> List Char -> List Frame
framesFromStrings accumulator chars =
    case chars of
        or :: sz :: rest ->
            { orientation =
                if or == 'H' then
                    Horizontal
                else
                    Vertical
            , scale = sizeFromString sz
            , mode = Showing
            }
                :: (framesFromStrings accumulator rest)

        _ ->
            accumulator

(||>) : Maybe a -> (a->b) -> Maybe b
(||>) ma f =
  Maybe.map f ma


modelFrom : Route -> Model
modelFrom (LayoutsRoute frames movieString) =
    { layout =
        { frames = frames |> Maybe.withDefault "VLHLHLVL" |> String.toList |> framesFromStrings [] |> Debug.log "frames"
        , movies = movieString ||> (String.split ",") ||> List.filterMap (findMovie movies) |> Maybe.withDefault []
        }
    }


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map LayoutsRoute (top <?> stringParam "frames" <?> stringParam "movies")
        ]


type Orientation
    = Horizontal
    | Vertical


type alias Movie =
    { fileName : String
    , orientation : Orientation
    , description : String
    }


moviesByOrientation : List Movie -> Orientation -> List Movie
moviesByOrientation movies orientation =
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


findMovie : List Movie -> String -> Maybe Movie
findMovie movies id =
    movies |> List.filter (\m -> m.fileName == "IMG_" ++ id ++ ".m4v") |> List.head


type Scale
    = Small
    | Medium
    | Large


type alias Layout =
    { frames : List Frame, movies : List Movie }


type VideoMode
    = Menu
    | Showing


type alias Frame =
    { orientation : Orientation, scale : Scale, mode : VideoMode }


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


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { layout : Layout }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( (parseHash route location) |> Debug.log "stuff" |> Maybe.withDefault (LayoutsRoute Nothing Nothing) |> modelFrom
    , Cmd.none
    )


type Msg
    = ShowMenu Movie
    | Swap Movie Movie
    | UrlChange Navigation.Location


swapMovie : Model -> Movie -> Movie -> Model
swapMovie model movie newMovie =
    let
        layout =
            model.layout
    in
        { model
            | layout =
                { movies = List.Extra.replaceIf ((==) movie) newMovie model.layout.movies
                , frames = (map (\f -> { f | mode = Showing }) model.layout.frames)
                }
        }


showMenu : Model -> Movie -> Model
showMenu model movie =
    let
        layout =
            model.layout

        index =
            Maybe.withDefault -1 (List.Extra.findIndex ((==) movie) model.layout.movies)

        changedFrames =
            List.indexedMap
                (\idx frame ->
                    { frame
                        | mode =
                            if idx == index then
                                Menu
                            else
                                Showing
                    }
                )
                model.layout.frames
    in
        { model | layout = { layout | frames = changedFrames } }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Swap movie newMovie ->
            ( (swapMovie model movie newMovie), Cmd.none )

        ShowMenu movie ->
            ( showMenu model movie, Cmd.none )

        UrlChange location ->
            ( model, Cmd.none )


(//) : Int -> Int -> String
(//) a b =
    toString a ++ "/" ++ toString b


movieItem : Movie -> Movie -> Html Msg
movieItem currentMovie subject =
    li [ onClick (Swap currentMovie subject) ] [ text subject.description ]


frameView : Frame -> Movie -> Html Msg
frameView frame currentMovie =
    case frame.mode of
        Showing ->
            video
                [ (loop True)
                , (onClick (ShowMenu currentMovie))
                , (src ("/public/" ++ currentMovie.fileName))
                , (autoplay True)
                ]
                []

        Menu ->
            ul [] (map (movieItem currentMovie) (moviesByOrientation movies currentMovie.orientation))


movieView : ( Movie, ( Frame, GridRectangle ) ) -> Html Msg
movieView ( movie, ( frame, gridRectangle ) ) =
    div
        [ (style
            [ ( "grid-row", gridRectangle.topRow // gridRectangle.bottomRow )
            , ( "grid-column", gridRectangle.leftColumn // gridRectangle.rightColumn )
            , ( "background-color", "#ccc" )
            , ( "padding", "5px" )
            ]
          )
        ]
        [ frameView frame movie ]


makeGrid =
    Grid.forType frameSize



-- TODO: 24 9 is a pretty random gridsize.
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
