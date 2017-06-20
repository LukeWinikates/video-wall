module App exposing (..)

import Html exposing (Html, b, div, li, text, ul, video, body)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (drop, foldl, head, indexedMap, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Geometry exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, oneOf, parseHash, (<?>), stringParam, top)
import Movie exposing (..)


-- TODO: simplify the Model: having layout nested doesn't seem to help anything
-- V-1-1-4-8,H-1-4-4-11,H-4-4-7-11,V-1-11-8-14


type alias GridFrame =
    { orientation : Orientation
    , top : Int
    , bottom : Int
    , left : Int
    , right : Int
    , mode : VideoMode
    }


toOrientation : Maybe String -> Orientation
toOrientation or =
    case or of
        Just "H" ->
            Horizontal

        _ ->
            Vertical


gridFrameFromStringArray : List String -> GridFrame
gridFrameFromStringArray list =
    case drop 1 list |> List.map String.toInt of
        (Ok t) :: (Ok l) :: (Ok b) :: (Ok r) :: [] ->
            { orientation = head list |> toOrientation, top = t, bottom = b, left = l, right = r, mode = Showing }

        _ ->
            { orientation = Vertical, top = 0, bottom = 0, left = 0, right = 0, mode = Showing }


parseGridFrames : String -> List GridFrame
parseGridFrames frameString =
    String.split "," frameString |> List.map (String.split "-") |> List.map gridFrameFromStringArray


type Route
    = LayoutsRoute (Maybe String) (Maybe String)


modelFrom : Route -> Model
modelFrom (LayoutsRoute maybeFrames maybeMovies) =
    case ( maybeFrames, maybeMovies ) of
        ( Just framesString, Just movieString ) ->
            { layout =
                { frames = framesString |> parseGridFrames
                , movies = movieString |> (String.split ",") |> List.filterMap findById
                }
            }

        _ ->
            { layout = { frames = [], movies = [] } }


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
    { frames : List GridFrame, movies : List Movie }


type VideoMode
    = Menu
    | Showing


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
    ( (parseHash route location) |> Maybe.withDefault (LayoutsRoute Nothing Nothing) |> modelFrom
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
                { layout | movies = (List.Extra.setAt index newMovie layout.movies) |> Maybe.withDefault model.layout.movies }
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


frameToString : GridFrame -> String
frameToString { orientation, top, left, bottom, right } =
    [ (case orientation of
        Horizontal ->
            "H"

        Vertical ->
            "V"
      )
    , toString top
    , toString left
    , toString bottom
    , toString right
    ]
        |> String.join "-"


framesUrlString : List GridFrame -> String
framesUrlString frames =
    frames |> List.map frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "?frames=" ++ (framesUrlString model.layout.frames) ++ "&movies=" ++ (model.layout.movies |> List.map Movie.id |> String.join ",")


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        wrap model =
            ( model, model |> toUrl |> Navigation.modifyUrl )
    in
        case action of
            Swap movie newMovie ->
                wrap (swapMovie model movie newMovie)

            ShowMenu index ->
                wrap (showMenu model index)

            UrlChange location ->
                ( model, Cmd.none )


movieItem : Int -> Movie -> Html Msg
movieItem index subject =
    li [ onClick (Swap index subject) ] [ text subject.description ]


frameView : GridFrame -> Int -> Movie -> Html Msg
frameView frame index movie =
    case frame.mode of
        Showing ->
            video
                [ (loop True)
                , (onClick (ShowMenu index))
                , (src ("/public/" ++ movie.fileName))
                , (style
                    [ ( case frame.orientation of
                            Horizontal ->
                                "max-height"

                            Vertical ->
                                "max-width"
                      , "100%"
                      )
                    , ( "border", "10px solid #3A3238" )
                    , ( "border-radius", "2px" )
                    ]
                  )
                , (autoplay True)
                ]
                []

        Menu ->
            ul [] (map (movieItem index) (byOrientation movie.orientation))


(+++) : number -> String -> String
(+++) i s =
    (toString i) ++ s


vwGrid : Int -> String
vwGrid i =
    ((toFloat i / 16) * 100) +++ "vw"


vhGrid : Int -> String
vhGrid i =
    ((toFloat i / 9) * 100) +++ "vh"


movieView : Int -> ( Movie, GridFrame ) -> Html Msg
movieView index ( movie, gridFrame ) =
    div
        [ (style
            [ ( "position", "absolute" )
            , ( "left", vwGrid (gridFrame.left - 1) )
            , ( "width", vwGrid (gridFrame.right - gridFrame.left) )
            , ( "top", vhGrid (gridFrame.top - 1) )
            , ( "height", vhGrid (gridFrame.bottom - gridFrame.top) )
            , ( "padding", "5px" )
            , ( "box-sizing", "border-box" )
            , ( "text-align", "center" )
            ]
          )
        ]
        [ frameView gridFrame index movie ]



-- TODO: make the grid actually work -- right now the videos are just full size, really.
-- TODO: seems like time to try abandoning CSS grid and absolutely positioning everything
-- the bummer is that means probably deciding on a fixed grid size and calculating fractional viewport widths, but I think I'm really not understanding CSS grid


view : Model -> Html Msg
view model =
    body
        [ (style
            []
          )
        ]
        [ div
            [ (style
                [ ( "display", "absolute" )
                , ( "height", "100vh" )
                , ( "width", "100vw" )
                , ( "background-color", "#636B61" )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "align-items", "center" )
                ]
              )
            ]
            (indexedMap movieView (zip model.layout.movies model.layout.frames))
        ]
