module App exposing (..)

import Html exposing (Html, a, b, body, div, li, text, ul, video)
import Html.Attributes exposing (autoplay, height, href, loop, src, style)
import Html.Events exposing (..)
import List exposing (drop, foldl, head, indexedMap, map, tail, take)
import List.Extra exposing (zip, last)
import Maybe exposing (withDefault)
import Tuple exposing (second)
import Geometry exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, oneOf, parseHash, (<?>), stringParam, top)
import Movie exposing (..)
import MovieParser exposing (..)


colors =
    { thunder = "#3A3238"
    , platinum = "#E2E2E2"
    , graniteGray = "#636B61"
    }


type alias GridMovie =
    { orientation : Orientation
    , top : Int
    , bottom : Int
    , left : Int
    , right : Int
    , mode : VideoMode
    , movie : Maybe Movie
    }


resultToMaybe : Result a b -> Maybe b
resultToMaybe =
    Result.map Just >> Result.withDefault Nothing


hydrate : MovieDefinition -> GridMovie
hydrate definition =
    { orientation = definition.orientation
    , top = definition.top
    , bottom = definition.bottom
    , left = definition.left
    , right = definition.right
    , movie = Movie.findById definition.movieId
    , mode = Showing
    }


parseGridFrames : String -> List GridMovie
parseGridFrames =
    String.split "," >> List.filterMap (MovieParser.parseMovie >> resultToMaybe) >> List.map hydrate


modelFrom : Route -> Model
modelFrom maybeMovies =
    case maybeMovies of
        Just movieString ->
            { movies = movieString |> parseGridFrames
            }

        _ ->
            { movies = [] }


route : Parser (Route -> a) a
route =
    (top <?> stringParam "movies")


type Scale
    = Small
    | Medium
    | Large


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
    { movies : List GridMovie }


type alias Route =
    Maybe String


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( (parseHash route location) |> Maybe.withDefault Nothing |> modelFrom
    , Cmd.none
    )


type Msg
    = ShowMenu Int
    | Swap Int Movie
    | UrlChange Navigation.Location


showAllMovies : Model -> Model
showAllMovies model =
    { model
        | movies = (map (\f -> { f | mode = Showing }) model.movies)
    }


swapMovie : Model -> Int -> Movie -> Model
swapMovie model index newMovie =
    { model
        | movies = (List.Extra.updateAt index (\m -> { m | movie = Just newMovie }) model.movies) |> Maybe.withDefault model.movies
    }
        |> showAllMovies


showMenu : Model -> Int -> Model
showMenu model index =
    { model
        | movies =
            List.Extra.updateAt index
                (\frame ->
                    { frame
                        | mode = Menu
                    }
                )
                model.movies
                |> Maybe.withDefault model.movies
    }


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
    , Maybe.map .id movie |> Maybe.withDefault "N"
    ]
        |> String.join "-"


framesUrlString : List GridMovie -> String
framesUrlString frames =
    frames |> List.map frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "?movies=" ++ (framesUrlString model.movies)


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
    li [ (style [ ( "padding", "4px" ) ]) ] [ a [ onClick (Swap index subject), (href "#"), (style [ ( "color", colors.thunder ), ( "font-size", "18px" ) ]) ] [ text subject.description ] ]


frameView : GridMovie -> Int -> Html Msg
frameView gridMovie index =
    case ( gridMovie.mode, gridMovie.movie ) of
        ( Showing, Just movie ) ->
            video
                [ (loop True)
                , (onClick (ShowMenu index))
                , (src ("/public/" ++ (fileName movie)))
                , (style
                    [ ( case gridMovie.orientation of
                            Horizontal ->
                                "max-height"

                            Vertical ->
                                "max-width"
                      , "100%"
                      )
                    , ( "border", "10px solid " ++ colors.thunder )
                    , ( "border-radius", "2px" )
                    , ( "margin", "auto" )
                    ]
                  )
                , (autoplay True)
                ]
                []

        _ ->
            ul
                [ (style
                    [ ( "background-color", colors.platinum )
                    , ( "width", "80%" )
                    , ( "padding", "10px" )
                    , ( "border", "10px solid " ++ colors.thunder )
                    , ( "border-radius", "2px" )
                    , ( "list-style", "none" )
                    , ( "margin", "auto" )
                    ]
                  )
                ]
                (map (movieItem index) (byOrientation gridMovie.orientation))


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
                , ( "background-color", colors.graniteGray )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "align-items", "center" )
                ]
              )
            ]
            (indexedMap movieView model.movies)
        ]
