module App exposing (..)

import Html exposing (Html, div, li, text, ul, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (foldl, head, indexedMap, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Grid exposing (..)
import Geometry exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, oneOf, parseHash, (<?>), stringParam, top)
import Movie exposing (..)


-- TODO: make movie changes correctly effect the url
-- TODO: simpliy the Model... model considerably


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


(||>) : Maybe a -> (a -> b) -> Maybe b
(||>) ma f =
    Maybe.map f ma


-- TODO: address defaults
modelFrom : Route -> Model
modelFrom (LayoutsRoute frames movieString) =
    { layout =
        { frames = frames |> Maybe.withDefault "VLHLHLVL" |> String.toList |> framesFromStrings [] |> Debug.log "frames"
        , movies = movieString ||> (String.split ",") ||> List.filterMap findById |> Maybe.withDefault []
        }
    }


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map LayoutsRoute (top <?> stringParam "frames" <?> stringParam "movies")
        ]


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
    = ShowMenu Int
    | Swap Int Movie
    | UrlChange Navigation.Location


showAllMovies : Model -> Model
showAllMovies model =
    let
        layout =
            model.layout
    in
        { model
            | layout =
                { layout
                    | frames = (map (\f -> { f | mode = Showing }) model.layout.frames)
                }
        }


swapMovie : Model -> Int -> Movie -> Model
swapMovie model index newMovie =
    let
        layout =
            model.layout
    in
        { model
            | layout =
                { layout | movies = (List.Extra.setAt (Debug.log "index" index) newMovie layout.movies) |> Maybe.withDefault model.layout.movies }
        }
            |> showAllMovies


showMenu : Model -> Int -> Model
showMenu model index =
    let
        layout =
            model.layout

        changedFrames =
            List.Extra.updateAt index
                (\frame ->
                    { frame
                        | mode = Menu
                    }
                )
                model.layout.frames
                |> Maybe.withDefault model.layout.frames
    in
        { model | layout = { layout | frames = changedFrames } }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Swap movie newMovie ->
            ( (swapMovie model movie newMovie), Cmd.none )

        ShowMenu index ->
            ( showMenu model index, Cmd.none )

        UrlChange location ->
            ( model, Cmd.none )


(//) : Int -> Int -> String
(//) a b =
    toString a ++ "/" ++ toString b


movieItem : Int -> Movie -> Html Msg
movieItem index subject =
    li [ onClick (Swap index subject) ] [ text subject.description ]


frameView : Frame -> Int -> Movie -> Html Msg
frameView frame index movie =
    case frame.mode of
        Showing ->
            video
                [ (loop True)
                , (onClick (ShowMenu index))
                , (src ("/public/" ++ movie.fileName))
                , (autoplay True)
                ]
                []

        Menu ->
            ul [] (map (movieItem index) (byOrientation movie.orientation))


movieView : Int -> ( Movie, ( Frame, GridRectangle ) ) -> Html Msg
movieView index ( movie, ( frame, gridRectangle ) ) =
    div
        [ (style
            [ ( "grid-row", gridRectangle.topRow // gridRectangle.bottomRow )
            , ( "grid-column", gridRectangle.leftColumn // gridRectangle.rightColumn )
            , ( "background-color", "#ccc" )
            , ( "padding", "5px" )
            ]
          )
        ]
        [ frameView frame index movie ]


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
            (indexedMap movieView (zip model.layout.movies grid.items))
