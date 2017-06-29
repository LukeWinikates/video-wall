module App exposing (..)

import Color
import FontAwesome
import Html exposing (Html, a, b, body, button, div, li, text, ul, video)
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
    , mistyRose = "#F5E3E0"
    }


type alias GridMovie =
    { orientation : Orientation
    , top : Int
    , height : Int
    , left : Int
    , width : Int
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
    , height = definition.height
    , left = definition.left
    , width = definition.width
    , movie = Movie.findById definition.movieId
    , mode = Showing
    }


gridMoviesFromUrlString : String -> List GridMovie
gridMoviesFromUrlString =
    String.split "," >> List.filterMap (MovieParser.parseMovie >> resultToMaybe) >> List.map hydrate


modelFrom : Route -> Model
modelFrom maybeMovies =
    { movies = maybeMovies |> (Maybe.map gridMoviesFromUrlString) |> (withDefault []) }


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
    | Buttons


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
    = ChangeMode VideoMode Int
    | Swap Int Movie
    | UrlChange Navigation.Location
    | Resize Scale Int
    | NewMovie Orientation


showAllMovies : Model -> Model
showAllMovies model =
    { model
        | movies = (map (\f -> { f | mode = Showing }) model.movies)
    }


changeMovieAtIndex : (GridMovie -> GridMovie) -> Model -> Int -> Model
changeMovieAtIndex f model index =
    { model
        | movies =
            model.movies
                |> List.Extra.updateAt index f
                |> Maybe.withDefault model.movies
    }


swapMovie : Model -> Int -> Movie -> Model
swapMovie model index newMovie =
    changeMovieAtIndex (\m -> { m | movie = Just newMovie }) model index
        |> showAllMovies


changeMode : Model -> VideoMode -> Int -> Model
changeMode model mode index =
    changeMovieAtIndex (\frame -> { frame | mode = mode }) model index


frameToString : GridMovie -> String
frameToString { orientation, top, left, height, width, movie } =
    [ (case orientation of
        Horizontal ->
            "H"

        Vertical ->
            "V"
      )
    , toString top
    , toString left
    , toString height
    , toString width
    , Maybe.map .id movie |> Maybe.withDefault "N"
    ]
        |> String.join "-"


framesUrlString : List GridMovie -> String
framesUrlString frames =
    frames |> List.map frameToString |> String.join ","


toUrl : Model -> String
toUrl model =
    "?movies=" ++ (framesUrlString model.movies)


type alias Dimension =
    { width : Int
    , height : Int
    }


getScale : Scale -> Orientation -> Dimension
getScale scale orientation =
    case ( scale, orientation ) of
        ( Small, Vertical ) ->
            { height = 340, width = 190 }

        ( Medium, Vertical ) ->
            { height = 516, width = 290 }

        ( Large, Vertical ) ->
            { height = 640, width = 360 }

        ( Small, Horizontal ) ->
            { height = 190, width = 340 }

        ( Medium, Horizontal ) ->
            { height = 290, width = 516 }

        ( Large, Horizontal ) ->
            { height = 360, width = 640 }


resizeMovie : Scale -> GridMovie -> GridMovie
resizeMovie scale gridMovie =
    let
        newScale =
            getScale scale gridMovie.orientation
    in
        { gridMovie | width = newScale.width, height = newScale.height }


resize : Scale -> Int -> Model -> Model
resize scale index model =
    changeMovieAtIndex (resizeMovie scale) model index


newMovie : Orientation -> Model -> Model
newMovie orientation model =
    { model
        | movies =
            model.movies
                ++ [ { orientation = orientation
                     , top = 500
                     , height = 1000
                     , left = 50
                     , width = 350
                     , movie = Nothing
                     , mode = Menu
                     }
                   ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        wrap model =
            ( model, model |> toUrl |> Navigation.modifyUrl )
    in
        case action of
            Swap index newMovie ->
                wrap (swapMovie model index newMovie)

            ChangeMode mode index ->
                wrap (changeMode model mode index)

            Resize scale index ->
                wrap (resize scale index model)

            NewMovie orientation ->
                wrap (newMovie orientation model)

            UrlChange location ->
                ( model, Cmd.none )


movieItem : Int -> Movie -> Html Msg
movieItem index subject =
    li [ (style [ ( "padding", "4px" ) ]) ] [ a [ onClick (Swap index subject), (href "#"), (style [ ( "color", colors.thunder ), ( "font-size", "18px" ) ]) ] [ text subject.description ] ]


changeButton : Msg -> String -> Html Msg
changeButton msg t =
    button [ (onClick msg), style [ ( "background-color", colors.mistyRose ), ( "border-radius", "2px" ), ( "border", "none" ), ( "margin", "5px" ), ( "padding", "5px 10px" ) ] ] [ (text t) ]


dragButton : Msg -> Html Msg -> Html Msg
dragButton msg icon =
    button [ (onClick msg), style [ ( "background-color", colors.mistyRose ), ( "border-radius", "2px" ), ( "border", "none" ), ( "margin", "5px" ), ( "padding", "5px 10px" ) ] ] [ icon ]


helperViews : GridMovie -> Int -> List (Html Msg)
helperViews gridMovie index =
    case gridMovie.mode of
        Buttons ->
            [ div [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ] ]
                [ dragButton (Resize Small 0) (FontAwesome.arrows Color.darkGray 12)
                , changeButton (Resize Small index) "S"
                , changeButton (Resize Medium index) "M"
                , changeButton (Resize Large index) "L"
                ]
            ]

        Menu ->
            [ ul
                [ (style
                    [ ( "background-color", colors.platinum )
                    , ( "width", "80%" )
                    , ( "padding", "10px" )
                    , ( "border", "10px solid " ++ colors.thunder )
                    , ( "border-radius", "2px" )
                    , ( "list-style", "none" )
                    , ( "margin", "auto" )
                    , ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "left", "0" )
                    ]
                  )
                ]
                (map (movieItem index) (byOrientation gridMovie.orientation))
            ]

        Showing ->
            []


videoTagView : Int -> Movie -> Html Msg
videoTagView index movie =
    video
        [ (loop True)
        , (onClick (ChangeMode Menu index))
        , (src ("/public/" ++ (fileName movie)))
        , (style
            [ ( case movie.orientation of
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


frameView : GridMovie -> Int -> Html Msg
frameView gridMovie index =
    div [ (onMouseEnter (ChangeMode Buttons index)), (onMouseLeave (ChangeMode Showing index)) ]
        ((List.filterMap identity [ (Maybe.map (videoTagView index) gridMovie.movie) ])
            ++ (helperViews gridMovie index)
        )


(+++) : number -> String -> String
(+++) i s =
    (toString i) ++ s


gridMovieView : Int -> GridMovie -> Html Msg
gridMovieView index gridMovie =
    div
        [ (style
            [ ( "position", "absolute" )
            , ( "left", gridMovie.left +++ "px" )
            , ( "width", gridMovie.width +++ "px" )
            , ( "top", gridMovie.top +++ "px" )
            , ( "height", gridMovie.height +++ "px" )
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
            (indexedMap gridMovieView model.movies)
        , changeButton (NewMovie Vertical) "+Vertical"
        , changeButton (NewMovie Horizontal) "+Horizontal"
        ]
