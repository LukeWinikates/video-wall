module App exposing (..)

import App.Buttons exposing (changeButton, dragButton, movieButton)
import App.Colors exposing (colors)
import App.Grid exposing (px, snap, videoBorderWidth)
import App.Msg exposing (Msg(..))
import App.SizePicker exposing (sizePickerView)
import App.Tray as Tray
import Color
import Dom.BackgroundClicker exposing (decodePosition, onClickElementWithId)
import Dom.Dragging as Dragging exposing (..)
import FontAwesome
import Geometry exposing (..)
import GuideLines exposing (guideLines)
import Html exposing (Attribute, Html, a, b, body, div, h2, li, p, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, property, rel, src, style)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode
import List exposing (drop, foldl, head, indexedMap, map, tail, take)
import Maybe exposing (withDefault)
import Model exposing (GridContent(..), GridItem, Model, TrayContent(MoviePicker, ShowingPoem), TrayMode(Collapsed, Expanded), gridItemsFromCommaSeparatedList)
import Model.MovieSwitcher
import Model.Mutate exposing (Mutation(..), applyAll, applyAtIndex, applyMutationAtIndex, changePosition, content, drag, newItem, remove, resize, setMovie, toggleVideoPicker)
import Model.Serialize exposing (toUrl)
import Mouse exposing (Position)
import Movie exposing (..)
import Navigation exposing (..)
import Primitives exposing (resultToMaybe)
import Time exposing (Time)
import UrlParser exposing (..)
import Dom.Video exposing (playbackRate, volume)
import Poem exposing (Poem)


-- TODO topic: sharing
-- TODO: something for saving curated collections/switching between collections, ala codepen
-- TODO topic: composition
-- TODO: when switching videos, highlight the ones that aren't already onscreen
-- TODO: add a "randomize" button
-- TODO topic: the grid / dragging :
-- TODO: maybe make final position snap to grid when dragging / updating url
-- TODO: when being dragged, the dragged item should have the highest z-index.
-- TODO topic: the item adding workflow
-- TODO: some kind of affordance indicating that clicking on the grid lets you add a movie
-- TODO: movie picker isn't that nice, and can't be closed
-- TODO: the different states for the process of adding an item feel disjointed
-- TODO topic: tray menu
-- TODO: store last interaction time when a mutation happens
-- TODO: hide tray menu icon when user hasn't interacted for a while
-- TODO topic: refactoring
-- TODO: look for duplication in styles, and find a way to make the latent structure more explicit
-- TODO category: user feedback
-- TODO: rotate button is confusing, because the first thing it does is make you pick a new video
-- TODO: provide preset layouts to start from?
-- TODO: size selector for Initial state when adding a new item is confusing
-- TODO: maybe reconsider interaction starting with clicking an arbitrary spot. Start with video selection instead, then position? The clicked position is probably just going to get adjusted later
-- TODO: an extra option that includes videos from all the collections
-- TODO: something that boosts z-index of last thing you touched, so that it stays on top
-- TODO: movie list often overflows the container. Display it fullscreen instead?
-- TODO: buttons don't have pointer style
-- TODO: links for changing the collection also don't have pointer style (is there a way to make the URL look right on the link, so ctrl+click works?)
-- TODO: landing from a bare url should show you something cool -> maybe the tray menu is open


type Route
    = AppRoute String String


route : Parser (Route -> a) a
route =
    UrlParser.map AppRoute (string </> string)


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( (parseHash route location) |> Maybe.map modelFrom |> Maybe.withDefault Model.empty
    , Cmd.none
    )


modelFrom : Route -> Model
modelFrom (AppRoute collectionId itemsString) =
    Maybe.withDefault Model.empty <|
        (Maybe.map
            (\collection ->
                { movies = itemsString |> gridItemsFromCommaSeparatedList collection
                , collection = collection
                , dragging = Nothing
                , trayMode = Collapsed
                }
            )
            (Movie.fromCollectionId
                collectionId
            )
        )


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
                        | movies = Model.MovieSwitcher.replaceMovies model.movies collectionName
                        , collection = collectionName
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


videoPicker : Int -> MovieCollection -> Orientation -> Html Msg
videoPicker index collection orientation =
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
        (List.map (movieItem index) (byOrientation collection orientation))
    )


helperViews : MovieCollection -> GridContent -> Int -> List (Html Msg)
helperViews collection content index =
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
                        collection
                        orientation
                    )
            )

        Picking orientation scale ->
            [ videoPicker index collection orientation ]

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


videoUrl : MovieCollection -> Movie -> String
videoUrl collection movie =
    ("/public/" ++ collection.id ++ "/" ++ (fileName movie))


videoTagView : Model -> Int -> Movie -> Html Msg
videoTagView model index movie =
    video
        [ (loop True)
        , (onClick (ChangeItem (ShowPicker True) index))
        , (src <| videoUrl model.collection movie)
        , (volume 0.005)
        , (playbackRate
            (if model.trayMode /= Collapsed then
                0.5
             else
                1.0
            )
          )
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
                    , (onMouseOver (ChangeItem (ShowHoverMenu True) index))
                    , (onMouseOut (ChangeItem (ShowHoverMenu False) index))
                    ]
                    ([ videoTagView model index movie ]
                        ++ (helperViews model.collection gridItem.content index)
                    )

            Initial preview ->
                sizePickerView gridItem preview index

            Picking orientation scale ->
                div
                    [ styles
                    ]
                    [ videoPicker index model.collection orientation ]


poemView : Poem -> Html Msg
poemView poem =
    div [ style [ ( "color", "white" ), ( "margin", ("10% 20%") ) ] ]
        ([ Html.h2 [ style [ ( "font-size", "24px" ) ] ] [ Html.text poem.title ]
         , Html.h3 [ style [ ( "font-size", "16px" ) ] ] [ Html.text poem.subtitle ]
         ]
            ++ List.map (\line -> p [ style [ ( "font-size", "18px" ) ] ] [ Html.text line ]) poem.lines
        )


movieFromGridItem : GridItem -> Maybe Movie
movieFromGridItem item =
    case item.content of
        Content _ _ m _ ->
            Just m

        _ ->
            Nothing


moviePickerView : Model -> Html Msg
moviePickerView model =
    let
        movies =
            Movie.except model.collection (List.filterMap movieFromGridItem model.movies)

        sizeForMovie =
            dimension Small
    in
        div [ style [ ( "display", "flex" ), ( "flex-wrap", "wrap" ), ( "background-color", colors.hex.mistyRose ) ] ]
            (List.map
                (\m ->
                    video
                        [ (volume 0)
                        , (autoplay True)
                        , src <| videoUrl model.collection m
                        , (style
                            [ ( "padding", "20px" )
                            , ( "height", sizeForMovie m.orientation |> .height |> px )
                            , ( "width", sizeForMovie m.orientation |> .width |> px )
                            ]
                          )
                        ]
                        []
                )
                movies
            )


overlayView : Model -> Html Msg
overlayView model =
    case model.trayMode of
        Expanded mode ->
            div
                [ style
                    [ ( "opacity", "0.4" )
                    , ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "left", "0" )
                    , ( "background-color", "black" )
                    , ( "height", "100vh" )
                    , ( "width", "100vw" )
                    ]
                ]
                [ case mode of
                    ShowingPoem ->
                        poemView <| Poem.poem model

                    MoviePicker ->
                        moviePickerView <| model
                ]

        Collapsed ->
            Html.text ""


view : Model -> Html Msg
view model =
    body
        []
        [ Html.node "link" [ href "https://fonts.googleapis.com/css?family=Lato", rel "stylesheet" ] []
        , div
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
                , ( "font-family", "'Lato', sans-serif" )
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
                , [ overlayView model ]
                , [ Tray.menuView model.trayMode ]
                ]
            )
        ]
