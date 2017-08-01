module App.Msg exposing (Msg(..))

import Dragging exposing (DragEvent)
import Model.Mutate exposing (Mutation)
import Mouse exposing (Position)
import Navigation


type Msg
    = ChangeMovie Mutation Int
    | UrlChange Navigation.Location
    | NewMovie Position
    | DragMovie Int DragEvent
    | Remove Int
