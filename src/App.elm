module App exposing (..)

import Html exposing (Html, div, li, text, ul, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (foldl, head, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Grid exposing (..)
import Geometry exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, oneOf, parseHash, (<?>), stringParam, top)
import Movie exposing (..)


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
            ul [] (map (movieItem currentMovie) (byOrientation currentMovie.orientation))


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
