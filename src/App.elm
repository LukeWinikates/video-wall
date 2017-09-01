module App exposing (..)

import App.Buttons exposing (changeButton, dragButton, movieButton)
import App.Colors exposing (colors, toCssColorString, transparentize)
import App.Grid exposing (px, snap, videoBorderWidth)
import App.GridEdges as GridEdges
import App.Msg exposing (Msg(..))
import App.Styles
import App.Routing
import App.Tray as Tray
import Color exposing (Color)
import Dom.BackgroundClicker exposing (decodePosition, onClickElementWithId)
import Dom.Dragging as Dragging exposing (..)
import FontAwesome
import Geometry exposing (..)
import GuideLines exposing (guideLines)
import Html exposing (Attribute, Html, a, b, body, div, h2, img, li, p, text, ul, video)
import Html.Attributes exposing (attribute, autoplay, height, href, loop, poster, property, rel, src, style)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode
import List exposing (drop, foldl, head, indexedMap, map, tail, take)
import Maybe exposing (withDefault)
import Model exposing (GridContent(..), GridItem, Model, TrayContent(MoviePicker, ShowingPoem), TrayMode(Collapsed, Expanded), empty, gridItemsFromCommaSeparatedList, dimensionsForContent)
import Model.MovieSwitcher
import Model.Mutate exposing (Mutation(..), applyAll, applyAtIndex, applyMutationAtIndex, changePosition, clearMenus, content, drag, hideTray, newItem, remove, resize, setMovie, toggleVideoPicker)
import Mouse exposing (Position)
import Movie exposing (..)
import Primitives exposing (resultToMaybe)
import Time exposing (Time)
import Dom.Video exposing (playbackRate, volume)
import Dom.ZIndexes as ZIndexes
import Poem exposing (Poem)
import Css
import Task


-- TODO topic: sharing
-- TODO: something for saving curated collections/switching between collections, ala codepen
-- TODO topic: composition
-- TODO: when switching videos, highlight the ones that aren't already onscreen
-- TODO: add a "randomize" button
-- TODO topic: the grid / dragging :
-- TODO: maybe make final position snap to grid when dragging / updating url
-- TODO: when being dragged, the dragged item should have the highest z-index.
-- TODO topic: refactoring
-- TODO: look for duplication in styles, and find a way to make the latent structure more explicit
-- TODO category: user feedback
-- TODO: rotate button is confusing, because the first thing it does is make you pick a new video
-- TODO: provide preset layouts to start from?
-- TODO: an extra option that includes videos from all the collections
-- TODO: something that boosts z-index of last thing you touched, so that it stays on top
-- TODO: movie list often overflows the container. Display it fullscreen instead?
-- TODO: links for changing the collection also don't have pointer style (is there a way to make the URL look right on the link, so ctrl+click works?)
-- TODO: landing from a bare url should show you something cool -> maybe the tray menu is open
-- TODO category: user feedback 2
-- TODO: positive feedback about rotate button keeping the active movie - how to use this?
-- TODO: when in movie picker mode, need a cancel button (maybe Esc works too?)
-- TODO: clicking tray menu button to dismiss menu is not obvious (maybe make it an X?, make it larger/animated?)
-- TODO category: general niceness
-- TODO: it's a little slow to load the videos when serving from GCP - what are some good options? (compression? cdn?)
-- TODO: unify tick-related events
-- TODO: capture the current screen size when initializing? use the screen size somehow to adjust the video size (as percentages/relative sizes?)
-- TODO: when panel opens up in response to interaction with an element, pick the left or right side dynamically depending on which keeps the element visible
-- TODO: on click, open panel to select video
-- TODO: consolidate duplicated styles for borders, positioning
-- TODO: hide the adders if the user doesn't interact for a while and the list is nonempty
-- TODO: drop the font awesome Elm package, and use the more conventional CSS font awesome version instead


main =
    App.Routing.program UrlChange
        { init = App.Routing.init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    [ (Dragging.subs model.dragging DragMovie) ]
        |> consIf
            (Model.userHasInteractedRecently model)
            (Time.every Time.second Tick)
        |> Sub.batch


wrapDrag : DragEventType -> Model -> ( Model, Cmd Msg )
wrapDrag typ model =
    case typ of
        End ->
            ( model, model |> App.Routing.modelToUrlCmd )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    let
        wrap model =
            ( model
            , model
                |> App.Routing.modelToUrlCmd
                |> ((flip (::)) [ Task.perform TrackUserInteraction Time.now ])
                |> Cmd.batch
            )
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

            NewMovie movie position ->
                wrap <| hideTray <| (newItem movie position model)

            TrayMenu mode ->
                ( { model | trayMode = mode }, Cmd.none )

            ChangeCollection collection ->
                wrap
                    ({ model
                        | movies = Model.MovieSwitcher.replaceMovies model.movies collection
                        , collection = collection
                     }
                    )

            DismissMenus ->
                wrap (applyAll clearMenus model)

            TrackUserInteraction time ->
                ( { model | lastInteractionTime = time, lastTick = time }, Cmd.none )

            Tick time ->
                ( { model | lastTick = time }, Cmd.none )

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


zIndexForContent : GridContent -> String
zIndexForContent (Content _ _ _ menus) =
    toString <|
        if menus.hoverMenu then
            ZIndexes.activeGridContent
        else
            ZIndexes.bottommost


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
            , ( "text-align", "center" )
            , ( "z-index", zIndexForContent item.content )
            ]
        )


videoTagView : Model -> Int -> Movie -> Html Msg
videoTagView model index movie =
    video
        [ (loop True)
        , (onClick (ChangeItem (ShowPicker True) index))
        , (src <| Movie.url model.collection movie)
        , (poster <| Movie.thumbnailUrl model.collection movie)
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


poemView : Poem -> Html Msg
poemView poem =
    div [ style [ ( "color", "white" ), ( "margin", ("10% 20%") ) ] ]
        ([ Html.h2 [ style [ ( "font-size", "24px" ) ] ] [ Html.text poem.title ]
         , Html.h3 [ style [ ( "font-size", "16px" ) ] ] [ Html.text poem.subtitle ]
         ]
            ++ List.map (\line -> p [ style [ ( "font-size", "18px" ) ] ] [ Html.text line ]) poem.lines
        )


movieFromGridItem : GridItem -> Movie
movieFromGridItem item =
    case item.content of
        Content _ _ m _ ->
            m


moviePickerView : Model -> Position -> Html Msg
moviePickerView model position =
    let
        movies =
            Movie.except model.collection (List.map movieFromGridItem model.movies)

        sizeForMovie =
            dimension Small
    in
        div
            [ style
                [ ( "background-color", colors.hex.mistyRose )
                , ( "overflow-y", "scroll" )
                , ( "width", "calc(100vw - 400px)" )
                , ( "height", "100%" )
                ]
            ]
            (List.map
                (\m ->
                    img
                        [ (src <| Movie.thumbnailUrl model.collection m)
                        , (onClick (NewMovie m position))
                          --                        , (onMouseEnter (TrayMenu (Expanded (MoviePicker { highlighted = Just m }))))
                          --                        , (onMouseLeave (TrayMenu (Expanded (MoviePicker { highlighted = Nothing }))))
                        , (style
                            [ ( "padding", "20px" )
                            , ( "margin", "20px" )
                            , ( "height", sizeForMovie m.orientation |> .height |> px )
                            , ( "width", sizeForMovie m.orientation |> .width |> px )
                            , ( "cursor", "pointer" )
                              --                            , ( "border"
                              --                              , (if model.trayMode == (Expanded (MoviePicker ({ highlighted = Just m }))) then
                              --                                    "2px solid " ++ colors.hex.thunder
                              --                                 else
                              --                                    "none"
                              --                                )
                              --                              )
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
                [ onClick (TrayMenu Collapsed)
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "left", "0" )
                    , ( "background-color", colors.color.thunder |> transparentize 0.9 |> toCssColorString )
                    , ( "height", "100vh" )
                    , ( "width", "100vw" )
                    ]
                ]
                [ case mode of
                    ShowingPoem ->
                        poemView <| Poem.poem model

                    MoviePicker pickerState ->
                        moviePickerView model pickerState.position
                ]

        Collapsed ->
            Html.text ""


movieAdder : Position -> Html Msg
movieAdder position =
    div
        [ (style
            ([ ( "position", "absolute" )
             , ( "left", position.x |> snap |> px )
             , ( "width", 200 |> snap |> px )
             , ( "top", position.y |> snap |> px )
             , ( "height", 200 |> snap |> px )
             , ( "text-align", "center" )
             , ( "border", "4px dashed " ++ colors.hex.thunder )
             , ( "border-radius", "2px" )
             , ( "background-color", colors.hex.graniteGray )
             , ( "color", colors.hex.thunder )
             , ( "cursor", "pointer" )
             ]
                ++ [ ( "display", "flex" )
                   , ( "justify-content", "center" )
                   , ( "align-items", "center" )
                   ]
            )
          )
        , onClick (TrayMenu (Expanded (MoviePicker { highlighted = Nothing, position = position })))
        ]
        [ div []
            [ Html.text "add a video"
            , p [] [ (FontAwesome.plus colors.color.thunder 24) ]
            ]
        ]


movieAdders : Model -> List (Html Msg)
movieAdders model =
    let
        padding =
            20
    in
        if List.isEmpty model.movies then
            [ movieAdder { x = padding, y = padding } ]
        else
            [ movieAdder { x = GridEdges.leftEdge model, y = padding + GridEdges.bottomEdge model }
            , movieAdder { x = padding + GridEdges.rightEdge model, y = GridEdges.topEdge model }
            ]


view : Model -> Html Msg
view model =
    body
        []
        [ Html.node "link" [ href "https://fonts.googleapis.com/css?family=Lato", rel "stylesheet" ] []
        , Html.node "style" [] [ text App.Styles.compiled ]
        , div
            [ Html.Attributes.id "background"
            , (onClickElementWithId "background" (Json.Decode.succeed DismissMenus) identity)
            , (style
                [ ( "display", "absolute" )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "align-items", "center" )
                ]
              )
            ]
            (List.concat
                [ (indexedMap (gridMovieView model) model.movies)
                , movieAdders model
                , ((Maybe.map
                        (always (guideLines model))
                        model.dragging
                   )
                    |> Maybe.withDefault []
                  )
                , [ overlayView model ]
                , [ Tray.menuView model ]
                ]
            )
        ]
