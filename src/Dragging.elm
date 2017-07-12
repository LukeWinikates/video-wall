module Dragging exposing (..)

import Mouse


type DragEventType
    = Start
    | Move
    | End


type DragEvent
    = DragEvent DragEventType Mouse.Position


type alias Drag draggedThingState =
    { current : { x : Int, y : Int }
    , state : draggedThingState
    }


subs : Maybe (Drag dragCapturedState) -> (dragCapturedState -> DragEvent -> appMsg) -> Sub appMsg
subs maybeDrag f =
    case maybeDrag of
        Nothing ->
            Sub.none

        Just { state } ->
            Sub.batch
                [ Mouse.moves (\p -> (f state (DragEvent Move p)))
                , Mouse.ups (\p -> (f state (DragEvent End p)))
                ]



-- TODO: can the drag state be avoided somehow? can it be eliminated from the model, moved into the event?


updateDrag : DragEvent -> a -> Maybe (Drag a) -> Maybe (Drag a)
updateDrag (DragEvent typ position) state maybeDrag =
    case typ of
        Start ->
            Just { current = position, state = state }

        Move ->
            Maybe.map (\drag -> { drag | current = position })
                maybeDrag

        End ->
            Nothing


map : DragEvent -> state -> Maybe (Drag state) -> (Mouse.Position -> Maybe (Drag state) -> model) -> model
map ((DragEvent typ position) as event) state maybeDrag f =
    (updateDrag event state maybeDrag)
        |> case ( typ, maybeDrag ) of
            ( Move, Just drag ) ->
                f
                    { y = position.y - drag.current.y
                    , x = position.x - drag.current.x
                    }

            _ ->
                f { y = 0, x = 0 }
