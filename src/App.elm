module App exposing (..)

import Color
import FontAwesome
import Html exposing (Attribute, Html, a, b, body, button, div, li, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, property, src, style)
import Html.Events exposing (..)
import Json.Encode
import List exposing (drop, foldl, head, indexedMap, map, tail, take)
import Maybe exposing (withDefault)
import Mouse exposing (Position)
import Geometry exposing (..)
import Navigation exposing (..)
import UrlParser exposing (Parser, parseHash, (<?>), stringParam, top)
import Movie exposing (..)
import MovieParser exposing (..)
import Json.Decode
import Primitives exposing (resultToMaybe)
import Model exposing (GridMovie, Model, Scale(..), VideoMode(..), gridMoviesFromUrlString, toUrl)
import Model.Mutate exposing (applyAll, applyAtIndex, changeMode, changePosition, drag, newMovie, resize, setMovie)
import Dragging exposing (..)
import List.Extra


-- TODO: something for saving curated collections/switching between collections, ala codepen
-- TODO: is there something cool to do with showing the name of the collection / the videos? (maybe an overlay that fades out?)
-- TODO: I'm interested in a snap-to-grid style, and maybe that also offers a solution?
-- TODO: maybe make final position snap to grid when dragging / updating url
-- TODO: adding video experience is not very good
-- TODO: click on an empty space to insert horizontal or vertical video
-- TODO: building up a layout from scratch is frustrating / if you change collections, there's no easy way to click to change the videos to valid ones for the collection


colors =
    { thunder = "#3A3238"
    , platinum = "#E2E2E2"
    , graniteGray = "#636B61"
    , mistyRose = "#F5E3E0"
    }


type Route
    = AppRoute (Maybe String) (Maybe String)


route : Parser (Route -> a) a
route =
    UrlParser.map AppRoute (top <?> stringParam "collection" <?> stringParam "movies")


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( (parseHash route location) |> Maybe.withDefault (AppRoute Nothing Nothing) |> modelFrom
    , Cmd.none
    )


modelFrom : Route -> Model
modelFrom (AppRoute maybeCollection maybeMovies) =
    case ( maybeCollection, maybeMovies ) of
        ( Just collectionName, Just movieString ) ->
            { movies = movieString |> gridMoviesFromUrlString collectionName
            , collection = collectionName
            , collectionMovies = Movie.fromCollection collectionName
            , dragging = Nothing
            }

        _ ->
            Model.empty


type Msg
    = ChangeMode VideoMode Int
    | Swap Int Movie
    | UrlChange Navigation.Location
    | Resize Scale Int
    | NewMovie Orientation
    | DragMovie Int DragEvent


subscriptions : Model -> Sub Msg
subscriptions model =
    Dragging.subs model.dragging DragMovie


wrapDrag : DragEventType -> Model -> ( Model, Cmd Msg )
wrapDrag typ model =
    case typ of
        End ->
            ( model, model |> toUrl |> Navigation.modifyUrl )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        wrap model =
            ( model, model |> toUrl |> Navigation.modifyUrl )
    in
        case action of
            Swap index newMovie ->
                wrap <|
                    applyAll (changeMode Showing) <|
                        (applyAtIndex (setMovie newMovie) index model)

            ChangeMode mode index ->
                wrap (applyAtIndex (changeMode mode) index model)

            Resize scale index ->
                wrap (resize scale index model)

            DragMovie index ((DragEvent typ _) as event) ->
                Dragging.map event
                    index
                    model.dragging
                    (\offset newDrag ->
                        (applyAtIndex (changePosition offset) index model)
                            |> drag newDrag
                    )
                    |> wrapDrag typ

            NewMovie orientation ->
                wrap (newMovie orientation model)

            UrlChange location ->
                ( model, Cmd.none )


movieItem : Int -> Movie -> Html Msg
movieItem index subject =
    li [ (style [ ( "padding", "4px" ) ]) ]
        [ a
            [ onClick (Swap index subject)
            , (href "#")
            , (style
                [ ( "color", colors.thunder )
                , ( "font-size", "18px" )
                ]
              )
            ]
            [ text subject.description
            ]
        ]


changeButton : Msg -> Html Msg -> Html Msg
changeButton msg content =
    button
        [ (onClick msg)
        , style
            [ ( "background-color", colors.mistyRose )
            , ( "border-radius", "2px" )
            , ( "border", "none" )
            , ( "margin", "5px" )
            , ( "padding", "5px 10px" )
            ]
        ]
        [ content
        ]


dragButton : (Mouse.Position -> Msg) -> Html Msg -> Html Msg
dragButton msg icon =
    button
        [ (onMouseDownWithDecoder msg)
        , style
            [ ( "background-color", colors.mistyRose )
            , ( "border-radius", "2px" )
            , ( "border", "none" )
            , ( "margin", "5px" )
            , ( "padding", "5px 10px" )
            ]
        ]
        [ icon ]


onMouseDownWithDecoder : (Mouse.Position -> Msg) -> Attribute Msg
onMouseDownWithDecoder f =
    on "mousedown" (Json.Decode.map f Mouse.position)


volume : Float -> Attribute msg
volume vol =
    (property "volume" (Json.Encode.string <| toString <| vol))


helperViews : List Movie -> GridMovie -> Int -> List (Html Msg)
helperViews collectionMovies gridMovie index =
    case gridMovie.mode of
        Buttons ->
            [ div [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ] ]
                [ dragButton (\p -> (DragMovie index (DragEvent Start p))) (FontAwesome.arrows Color.darkGray 12)
                , changeButton (Resize Small index) (text "S")
                , changeButton (Resize Medium index) (text "M")
                , changeButton (Resize Large index) (text "L")
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
                (List.map (movieItem index) (byOrientation collectionMovies gridMovie.orientation))
            ]

        Showing ->
            []


videoTagView : Model -> Int -> Movie -> Html Msg
videoTagView model index movie =
    video
        [ (loop True)
        , (onClick (ChangeMode Menu index))
        , (src ("/public/" ++ model.collection ++ "/" ++ (fileName movie)))
        , (volume 0.2)
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


px : a -> String
px =
    toString >> (flip (++) "px")


snap : Int -> Int
snap value =
    (round (toFloat value / 20)) * 20



-- draw relevant gridlines.
-- when within 10 px of a gridline (left, vcenter, right, hcenter, top, bottom), show a
--- dotted line indicating that they might want to snap to this
-- or... snap to gridline on drop.


gridMovieView : Model -> Int -> GridMovie -> Html Msg
gridMovieView model index gridMovie =
    div
        [ (style
            [ ( "position", "absolute" )
            , ( "left", gridMovie.left |> snap |> px )
            , ( "width", gridMovie.width |> snap |> px )
            , ( "top", gridMovie.top |> snap |> px )
            , ( "height", gridMovie.height |> snap |> px )
            , ( "padding", "5px" )
            , ( "box-sizing", "border-box" )
            , ( "text-align", "center" )
            , ( "z-index"
              , case gridMovie.mode of
                    Buttons ->
                        "20"

                    _ ->
                        "0"
              )
            ]
          )
        , (onMouseEnter (ChangeMode Buttons index))
        , (onMouseLeave (ChangeMode Showing index))
        ]
        ((List.filterMap identity [ (Maybe.map (videoTagView model index) gridMovie.movie) ])
            ++ (helperViews model.collectionMovies gridMovie index)
        )


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
            (indexedMap (gridMovieView model) model.movies)
        , changeButton (NewMovie Vertical) (text "+Vertical")
        , changeButton (NewMovie Horizontal) (text "+Horizontal")
        ]
