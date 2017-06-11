module App exposing (..)

import Html exposing (Html, div, li, text, ul, video)
import Html.Attributes exposing (autoplay, height, loop, src, style)
import Html.Events exposing (..)
import List exposing (foldl, head, indexedMap, map, tail, take)
import List.Extra exposing (zip, last)
import Tuple exposing (second)
import Geometry exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, oneOf, parseHash, (<?>), stringParam, top)
import Movie exposing (..)


-- TODO: simplify the Model: having layout nested doesn't seem to help anything

-- V-1-1-4-8,H-1-4-4-11,H-4-4-7-11,V-1-11-8-14

type alias GridFrame =
  {
    orientation: Orientation
    , top: String
    , bottom: String
    , left: String
    , right: String
    , mode: VideoMode
  }

toOrientation : String -> Orientation
toOrientation or = if or == "H" then
                    Horizontal
                else
                    Vertical

gridFrameFromStringArray : List String -> GridFrame
gridFrameFromStringArray l =
  case l of
    o::t::l::b::r::[] -> { orientation = o |> toOrientation, top = t, bottom = b, left = l, right = r, mode = Showing }
    _ -> { orientation = Vertical, top = "0", bottom = "0", left = "0", right = "0", mode = Showing}


parseGridFrames : String -> List GridFrame
parseGridFrames frameString =
  String.split "," frameString |> List.map (String.split "-") |> List.map gridFrameFromStringArray

type Route
    = LayoutsRoute (Maybe String) (Maybe String)

modelFrom : Route -> Model
modelFrom (LayoutsRoute maybeFrames maybeMovies) =
  case (maybeFrames, maybeMovies) of
    (Just framesString, Just movieString) ->
      { layout =
          { frames = framesString |> parseGridFrames
          , movies = movieString |> (String.split ",") |> List.filterMap findById
          }
      }
    _ -> { layout = { frames = [], movies = []}}



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
    [(case orientation of
        Horizontal ->
            "H"

        Vertical ->
            "V"
    ), top, left, bottom, right] |> String.join "-"


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


(//) : String -> String -> String
(//) a b =
    a ++ "/" ++ b


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
                , (autoplay True)
                ]
                []

        Menu ->
            ul [] (map (movieItem index) (byOrientation movie.orientation))


movieView : Int -> ( Movie, GridFrame ) -> Html Msg
movieView index ( movie, gridFrame ) =
    div
        [ (style
            [ ( "grid-row", gridFrame.top // gridFrame.bottom )
            , ( "grid-column", gridFrame.left // gridFrame.right )
            , ( "background-color", "#3A3238" )
            , ( "border-radius", "2px")
            , ( "padding", "5px" )
            , ( "box-sizing", "border-box" )
            ]
          )
        ]
        [ frameView gridFrame index movie ]


-- TODO: make the grid actually work -- right now the videos are just full size, really.

view : Model -> Html Msg
view model =
      div
          [ (style
              [ ( "display", "grid" )
              , ( "grid-gap", "10px" )
              , ( "background-color", "#636B61" )
              , ( "justify-items", "center" )
              , ( "align-items", "center" )
              ]
            )
          ]
          (indexedMap movieView (zip model.layout.movies model.layout.frames))
