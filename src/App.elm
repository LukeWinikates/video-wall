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
-- V-2-1-8-5-6259,H-1-5-6-12-6219,H-6-5-10-12-6259,V-2-12-8-16-6260

type alias GridMovie =
    { orientation : Orientation
    , top : Int
    , bottom : Int
    , left : Int
    , right : Int
    , mode : VideoMode
    , movie : Maybe Movie
    }


toOrientation : Maybe String -> Orientation
toOrientation or =
    case or of
        Just "H" ->
            Horizontal

        _ ->
            Vertical


gridFrameFromStringArray : List String -> GridMovie
gridFrameFromStringArray list =
    case drop 1 list |> take 4 |> List.map String.toInt of
        (Ok t) :: (Ok l) :: (Ok b) :: (Ok r) :: [] ->
            { orientation = head list |> toOrientation,
            top = t,
            bottom = b,
            left = l,
            right = r,
            mode = Showing,
            movie = List.Extra.last list |> Maybe.andThen findById
            }

        _ ->
            { orientation = Vertical, top = 0, bottom = 0, left = 0, right = 0, mode = Showing, movie = Nothing }


parseGridFrames : String -> List GridMovie
parseGridFrames frameString =
    String.split "," frameString |> List.map (String.split "-") |> List.map gridFrameFromStringArray


type Route
    = LayoutsRoute (Maybe String)


modelFrom : Route -> Model
modelFrom (LayoutsRoute maybeMovies) =
    case (maybeMovies ) of
        ( Just movieString ) ->
            { layout =
                { movies = movieString |> parseGridFrames
                }
            }

        _ ->
            { layout = { movies = [] } }


route : Parser (Route -> a) a
route =
    oneOf
        [ UrlParser.map LayoutsRoute (top <?> stringParam "movies")
        ]


type Scale
    = Small
    | Medium
    | Large


type alias Layout =
    { movies : List GridMovie }


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
    ( (parseHash route location) |> Maybe.withDefault (LayoutsRoute Nothing) |> modelFrom
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
                    | movies = (map (\f -> { f | mode = Showing }) model.layout.movies)
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
                { layout | movies = (List.Extra.updateAt index (\m -> { m | movie = Just newMovie}) layout.movies) |> Maybe.withDefault model.layout.movies }
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
                model.layout.movies
                |> Maybe.withDefault model.layout.movies
    in
        { model | layout = { layout | movies = changedFrames } }


frameToString : GridMovie -> String
frameToString { orientation, top, left, bottom, right, movie } =
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
    , Maybe.map Movie.id movie |> Maybe.withDefault "N"
    ]
        |> String.join "-"


framesUrlString : List GridMovie -> String
framesUrlString frames =
    frames |> List.map frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "?movies==" ++ (framesUrlString model.layout.movies)


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


frameView : GridMovie -> Int -> Html Msg
frameView gridMovie index =
    case (gridMovie.mode, gridMovie.movie) of
        (Showing, Just movie) ->
            video
                [ (loop True)
                , (onClick (ShowMenu index))
                , (src ("/public/" ++ movie.fileName))
                , (style
                    [ ( case gridMovie.orientation of
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

        _ ->
            ul [] (map (movieItem index) (byOrientation gridMovie.orientation))


(+++) : number -> String -> String
(+++) i s =
    (toString i) ++ s


vwGrid : Int -> String
vwGrid i =
    ((toFloat i / 16) * 100) +++ "vw"


vhGrid : Int -> String
vhGrid i =
    ((toFloat i / 9) * 100) +++ "vh"


movieView : Int -> GridMovie -> Html Msg
movieView index gridMovie =
    div
        [ (style
            [ ( "position", "absolute" )
            , ( "left", vwGrid (gridMovie.left - 1) )
            , ( "width", vwGrid (gridMovie.right - gridMovie.left) )
            , ( "top", vhGrid (gridMovie.top - 1) )
            , ( "height", vhGrid (gridMovie.bottom - gridMovie.top) )
            , ( "padding", "5px" )
            , ( "box-sizing", "border-box" )
            , ( "text-align", "center" )
            ]
          )
        ]
        [ frameView gridMovie index ]



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
            (indexedMap movieView model.layout.movies)
        ]
