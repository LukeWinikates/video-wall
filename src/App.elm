module App exposing (..)

import App.Buttons exposing (changeButton, dragButton, movieButton)
import App.Colors exposing (colors)
import App.Grid exposing (px, snap, videoBorderWidth)
import App.Msg exposing (Msg(..))
import App.SizePicker exposing (sizePickerView)
import BackgroundClicker exposing (decodePosition, onClickElementWithId)
import Color
import Dragging exposing (..)
import FontAwesome
import Geometry exposing (..)
import GuideLines exposing (guideLines)
import Html exposing (Attribute, Html, a, b, body, button, div, h2, li, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, property, src, style)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode
import List exposing (drop, foldl, head, indexedMap, map, tail, take)
import Maybe exposing (withDefault)
import Model exposing (GridContent(..), GridItem, Model, TrayMode(Collapsed, Expanded), gridMoviesFromUrlString)
import Model.Mutate exposing (Mutation(..), applyAll, applyAtIndex, applyMutationAtIndex, changePosition, content, drag, newItem, remove, resize, setMovie, toggleVideoPicker)
import Model.Serialize exposing (toUrl)
import Mouse exposing (Position)
import Movie exposing (..)
import Navigation exposing (..)
import Primitives exposing (resultToMaybe)
import Time exposing (Time)
import UrlParser exposing (Parser, parseHash, (<?>), stringParam, top)


-- TODO: something for saving curated collections/switching between collections, ala codepen
-- TODO: is there something cool to do with showing the name of the collection / the videos? (maybe an overlay that fades out?)
-- TODO: maybe when the menu is open, there's a translucent overlay, playback speed is slowed, and
--        the collection name and left -> right top -> bottom list of movie titles shows up
-- TODO: when the collection switches, randomly populate the videos
-- TODO: when switching videos, highlight the ones that aren't already onscreen
-- TODO: maybe make final position snap to grid when dragging / updating url
-- TODO: when being dragged, the dragged item should have the highest z-index.
-- TODO: when should the snapping *actually* happen?
-- TODO: store last interaction time when a mutation happens
-- TODO: some kind of affordance indicating that clicking on the grid lets you add a movie
-- TODO: movie picker isn't that nice, and can't be closed
-- TODO: the different states for the process of adding an item feel disjointed


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
            , trayMode = Collapsed
            }

        _ ->
            Model.empty


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
            ChangeItem mutation index ->
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

            NewMovie position ->
                wrap (newItem position model)

            TrayMenu mode ->
                ( { model | trayMode = mode }, Cmd.none )

            ChangeCollection collectionName ->
                wrap
                    ({ model
                        | movies = []
                        , collection = collectionName
                        , collectionMovies = Movie.fromCollection collectionName
                     }
                    )

            UrlChange location ->
                ( model, Cmd.none )


movieItem : Int -> Movie -> Html Msg
movieItem index subject =
    li [ (style [ ( "padding", "4px" ) ]) ]
        [ a
            [ onClick (ChangeItem (Swap subject) index)
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


volume : Float -> Attribute msg
volume vol =
    (property "volume" (Json.Encode.string <| toString <| vol))


consIf : Bool -> a -> List a -> List a
consIf condition item items =
    if condition then
        items ++ [ item ]
    else
        items


hoverMenu : Int -> Orientation -> Html Msg
hoverMenu index orientation =
    (div
        [ style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ] ]
        [ div []
            [ dragButton ((DragEvent Start) >> (DragMovie index)) (FontAwesome.arrows colors.color.thunder 12)
            , changeButton (ChangeItem (Rotate orientation) index) (FontAwesome.undo colors.color.thunder 12)
            , changeButton (Remove index) (FontAwesome.close colors.color.thunder 12)
            ]
        , div []
            [ changeButton (ChangeItem (Resize Small) index) (text "S")
            , changeButton (ChangeItem (Resize Medium) index) (text "M")
            , changeButton (ChangeItem (Resize Large) index) (text "L")
            ]
        ]
    )


videoPicker : Int -> List Movie -> Orientation -> Html Msg
videoPicker index collectionMovies orientation =
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
        (List.map (movieItem index) (byOrientation collectionMovies orientation))
    )


helperViews : List Movie -> GridContent -> Int -> List (Html Msg)
helperViews collectionMovies content index =
    case content of
        Content orientation scale movie menus ->
            ([]
                |> consIf menus.hoverMenu
                    (hoverMenu
                        index
                        orientation
                    )
                |> consIf menus.videoPicker
                    (videoPicker
                        index
                        collectionMovies
                        orientation
                    )
            )

        Picking orientation scale ->
            [ videoPicker index collectionMovies orientation ]

        _ ->
            []


dimensionsForContent : GridContent -> Dimension
dimensionsForContent content =
    let
        ( orientation, scale ) =
            case content of
                Picking orientation scale ->
                    ( orientation, scale )

                Content orientation scale _ _ ->
                    ( orientation, scale )

                _ ->
                    ( Vertical, Medium )
    in
        dimension scale orientation


zIndexForContent : GridContent -> String
zIndexForContent content =
    toString <|
        case content of
            Content _ _ _ menus ->
                if menus.hoverMenu then
                    20
                else
                    0

            _ ->
                0


gridItemStyling : GridItem -> Attribute Msg
gridItemStyling item =
    let
        { height, width } =
            dimensionsForContent item.content
    in
        (style
            [ ( "position", "absolute" )
            , ( "left", item.left |> snap |> px )
            , ( "width", width |> snap |> px )
            , ( "top", item.top |> snap |> px )
            , ( "height", height |> snap |> px )
            , ( "box-sizing", "border-box" )
            , ( "text-align", "center" )
            , ( "z-index", zIndexForContent item.content )
            ]
        )


videoTagView : Model -> Int -> Movie -> Html Msg
videoTagView model index movie =
    video
        [ (loop True)
        , (onClick (ChangeItem (ShowPicker True) index))
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


gridMovieView : Model -> Int -> GridItem -> Html Msg
gridMovieView model index gridItem =
    let
        styles =
            gridItemStyling gridItem
    in
        case gridItem.content of
            Content orientation scale movie menus ->
                div
                    [ styles
                    , (onMouseEnter (ChangeItem (ShowHoverMenu True) index))
                    , (onMouseLeave (ChangeItem (ShowHoverMenu False) index))
                    ]
                    ([ videoTagView model index movie ]
                        ++ (helperViews model.collectionMovies gridItem.content index)
                    )

            Initial preview ->
                sizePickerView gridItem preview index

            Picking orientation scale ->
                div
                    [ styles
                    ]
                    [ videoPicker index model.collectionMovies orientation ]


collectionSwitchLink : String -> Html Msg
collectionSwitchLink collectionName =
    div [] [ a [ onClick (ChangeCollection collectionName) ] [ Html.text collectionName ] ]


menuView : TrayMode -> Html Msg
menuView mode =
    case mode of
        Collapsed ->
            div
                [ style [ ( "position", "absolute" ), ( "top", 20 |> px ), ( "right", 20 |> px ) ] ]
                [ changeButton (TrayMenu Expanded) (FontAwesome.gear colors.color.thunder 12) ]

        Expanded ->
            div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", 0 |> px )
                    , ( "right", 0 |> px )
                    , ( "padding", 20 |> px )
                    , ( "height", "100vh" )
                    , ( "width", "400px" )
                    , ( "background-color", colors.hex.mistyRose )
                    , ( "border-left", "2px solid " ++ colors.hex.thunder )
                    ]
                ]
                ([ changeButton (TrayMenu Collapsed) (FontAwesome.arrow_right colors.color.thunder 12)
                 , h2 [] [ Html.text "Collections" ]
                 ]
                    ++ (List.map collectionSwitchLink Movie.collections)
                )


view : Model -> Html Msg
view model =
    body
        []
        [ div
            [ Html.Attributes.id "background"
            , (onClickElementWithId "background" decodePosition NewMovie)
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
            (List.concat
                [ (indexedMap (gridMovieView model) model.movies)
                , ((Maybe.map
                        (always (guideLines model))
                        model.dragging
                   )
                    |> Maybe.withDefault []
                  )
                , [ menuView model.trayMode ]
                ]
            )
        ]
