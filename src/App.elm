module App exposing (..)

import BackgroundClicker exposing (decodePosition, onClickElementWithId)
import Color
import DomHelpers exposing (px, snap)
import FontAwesome
import GuideLines exposing (guideLines)
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
import Json.Decode exposing (Decoder)
import Primitives exposing (resultToMaybe)
import Model exposing (GridMovie, Model, Scale(..), VideoMode(..), gridMoviesFromUrlString, toUrl)
import Model.Mutate exposing (Mutation(..), applyAll, applyAtIndex, applyMutationAtIndex, changeMode, changePosition, drag, newMovie, resize, setMovie, remove)
import Dragging exposing (..)


-- TODO: something for saving curated collections/switching between collections, ala codepen
-- TODO: building up a layout from scratch is frustrating / if you change collections, there's no easy way to click to change the videos to valid ones for the collection
-- TODO: is there something cool to do with showing the name of the collection / the videos? (maybe an overlay that fades out?)
-- TODO: I'm interested in a snap-to-grid style, and maybe that also offers a solution?
-- TODO: maybe make final position snap to grid when dragging / updating url
-- TODO: because the scale is not captured directly, and is instead encoded as the height/width measure, when the movie gets rotated it's not easy to go from height/width back to scale, and preserving scale is more annoying than it should be. Save the scale instead of the height/width.
-- TODO: when being dragged, the dragged item should have the highest z-index.
-- TODO: eliminate error state when movie is Nothing and the element becomes unhoverable
-- TODO: when creating a new video, first show size and orientation picker, then show video picker
-- TODO: when creating a new video, the object is not a video yet - it's a pre-video
-- TODO: menu for switching between collections
-- TODO: modal video adder? scroll into view?
-- TODO: when should the snapping *actually* happen?


colors =
    { hex =
        { thunder = "#3A3238"
        , platinum = "#E2E2E2"
        , graniteGray = "#636B61"
        , mistyRose = "#F5E3E0"
        }
    , color =
        { thunder = Color.rgb 58 50 56
        , platinum = Color.rgb 226 226 226
        , graniteGray = Color.rgb 99 107 97
        , mistyRose = Color.rgb 245 227 224
        }
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
    = ChangeMovie Mutation Int
    | UrlChange Navigation.Location
    | NewMovie Orientation Scale Position
    | DragMovie Int DragEvent
    | Remove Int


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
            ChangeMovie mutation index ->
                wrap (applyMutationAtIndex mutation index model)

            Remove index ->
                wrap (remove index model)

            DragMovie index ((DragEvent typ _) as event) ->
                Dragging.map event
                    index
                    model.dragging
                    (\offset newDrag ->
                        (applyAtIndex (changePosition offset) index model)
                            |> drag newDrag
                    )
                    |> wrapDrag typ

            NewMovie orientation size position ->
                wrap (newMovie orientation size position model)

            UrlChange location ->
                ( model, Cmd.none )


movieItem : Int -> Movie -> Html Msg
movieItem index subject =
    li [ (style [ ( "padding", "4px" ) ]) ]
        [ a
            [ onClick (ChangeMovie (Swap subject) index)
            , (href "#")
            , (style
                [ ( "color", colors.hex.thunder )
                , ( "font-size", "18px" )
                ]
              )
            ]
            [ text subject.description
            ]
        ]


movieButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
movieButton attributes content =
    button
        (attributes
            ++ [ style
                    [ ( "background-color", colors.hex.mistyRose )
                    , ( "border-radius", "2px" )
                    , ( "color", colors.hex.thunder )
                    , ( "min-width", "3em" )
                    , ( "font-weight", "bold" )
                    , ( "height", "24px" )
                    , ( "border", "none" )
                    , ( "margin", "5px" )
                    , ( "padding", "5px 10px" )
                    ]
               ]
        )
        content


changeButton : Msg -> Html Msg -> Html Msg
changeButton msg content =
    movieButton
        [ onClick msg ]
        [ content ]


dragButton : (Mouse.Position -> Msg) -> Html Msg -> Html Msg
dragButton msg icon =
    movieButton
        [ onMouseDownWithDecoder msg ]
        [ icon ]


onMouseDownWithDecoder : (Mouse.Position -> Msg) -> Attribute Msg
onMouseDownWithDecoder f =
    on "mousedown" (Json.Decode.map f Mouse.position)


volume : Float -> Attribute msg
volume vol =
    (property "volume" (Json.Encode.string <| toString <| vol))


consIf : Bool -> a -> List a -> List a
consIf condition item items =
    if condition then
        items ++ [ item ]
    else
        items


helperViews : List Movie -> GridMovie -> Int -> List (Html Msg)
helperViews collectionMovies gridMovie index =
    []
        |> consIf gridMovie.menu
            (div
                [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ] ]
                [ div []
                    [ dragButton (\p -> (DragMovie index (DragEvent Start p))) (FontAwesome.arrows colors.color.thunder 12)
                    , changeButton (ChangeMovie (Rotate gridMovie.orientation) index) (FontAwesome.undo colors.color.thunder 12)
                    , changeButton (Remove index) (FontAwesome.close colors.color.thunder 12)
                    ]
                , div []
                    [ changeButton (ChangeMovie (Resize Small) index) (text "S")
                    , changeButton (ChangeMovie (Resize Medium) index) (text "M")
                    , changeButton (ChangeMovie (Resize Large) index) (text "L")
                    ]
                ]
            )
        |> consIf (gridMovie.mode == Menu)
            (ul
                [ (style
                    [ ( "background-color", colors.hex.platinum )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "padding", "10px" )
                    , ( "border", "10px solid " ++ colors.hex.thunder )
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
            )


videoTagView : Model -> Int -> Movie -> Html Msg
videoTagView model index movie =
    let
        videoBorderWidth =
            10
    in
        video
            [ (loop True)
            , (onClick (ChangeMovie (ChangeMode Menu) index))
            , (src ("/public/" ++ model.collection ++ "/" ++ (fileName movie)))
            , (volume 0.005)
            , (style
                [ ( case movie.orientation of
                        Horizontal ->
                            "max-height"

                        Vertical ->
                            "max-width"
                  , "calc(100% - " ++ ((2 * videoBorderWidth) |> px) ++ ")"
                  )
                , ( "border", (videoBorderWidth |> px) ++ " solid " ++ colors.hex.thunder )
                , ( "border-radius", "2px" )
                , ( "margin", "auto" )
                ]
              )
            , (autoplay True)
            ]
            []


gridMovieView : Model -> Int -> GridMovie -> Html Msg
gridMovieView model index gridMovie =
    div
        [ (style
            [ ( "position", "absolute" )
            , ( "left", gridMovie.left |> snap |> px )
            , ( "width", gridMovie.width |> snap |> px )
            , ( "top", gridMovie.top |> snap |> px )
            , ( "height", gridMovie.height |> snap |> px )
            , ( "box-sizing", "border-box" )
            , ( "text-align", "center" )
            , ( "z-index"
              , if gridMovie.menu then
                    "20"
                else
                    "0"
              )
            ]
          )
        , (onMouseEnter (ChangeMovie (ToggleMenu True) index))
        , (onMouseLeave (ChangeMovie (ChangeMode Showing) index))
        ]
        ((List.filterMap identity [ (Maybe.map (videoTagView model index) gridMovie.movie) ])
            ++ (helperViews model.collectionMovies gridMovie index)
        )


view : Model -> Html Msg
view model =
    body
        []
        [ div
            [ Html.Attributes.id "background"
            , (onClickElementWithId "background" decodePosition (NewMovie Vertical Large))
            , (style
                [ ( "display", "absolute" )
                , ( "height", "100vh" )
                , ( "width", "100vw" )
                , ( "background-color", colors.hex.graniteGray )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "align-items", "center" )
                ]
              )
            ]
            (List.append
                (indexedMap (gridMovieView model) model.movies)
                ((Maybe.map
                    (always (guideLines model))
                    model.dragging
                 )
                    |> Maybe.withDefault []
                )
            )
        ]
