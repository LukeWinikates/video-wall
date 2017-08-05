module App.Msg exposing (Msg(..))

import Dom.Dragging exposing (DragEvent)
import Model exposing (TrayMode)
import Model.Mutate exposing (Mutation)
import Mouse exposing (Position)
import Movie exposing (MovieCollection)
import Navigation


type Msg
    = ChangeItem Mutation Int
    | UrlChange Navigation.Location
    | NewMovie Position
    | DragMovie Int DragEvent
    | Remove Int
    | TrayMenu TrayMode
    | ChangeCollection MovieCollection
