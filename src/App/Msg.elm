module App.Msg exposing (Msg(..))

import Dom.Dragging exposing (DragEvent)
import Model exposing (TrayMode)
import Model.Mutate exposing (Mutation)
import Mouse exposing (Position)
import Movie exposing (Movie, MovieCollection)
import Navigation
import Time exposing (Time)


type Msg
    = ChangeItem Mutation Int
    | UrlChange Navigation.Location
    | NewMovie Position Movie
    | DragMovie Int DragEvent
    | Remove Int
    | TrayMenu TrayMode
    | ChangeCollection MovieCollection
    | DismissMenus
    | TrackUserInteraction Time
    | Tick Time
