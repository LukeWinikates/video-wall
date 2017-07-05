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
    , start : { x : Int, y : Int }
    , state : draggedThingState
    }


subs : Maybe (Drag dragCapturedState) -> (dragCapturedState -> DragEvent -> appMsg) -> Sub appMsg
subs maybeDrag f =
    case maybeDrag of
        Nothing ->
            Sub.none

        Just { state } ->
            Sub.batch
                [ Mouse.moves (\p -> (f state (DragEvent Start p)))
                , Mouse.ups (\p -> (f state (DragEvent End p)))
                ]

-- TODO: it feels like the dragged state is not really working all that well,
-- since it still needs to be passed through here.

updateDrag : DragEvent -> a -> Maybe (Drag a) -> Maybe (Drag a)
updateDrag (DragEvent typ position) state maybeDrag =
    case typ of
        Start ->
            Just { current = position, start = position, state = state }

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
                    { y = position.y + drag.current.y - drag.start.y
                    , x = position.x + drag.current.x - drag.start.x
                    }

            _ ->
                f position
