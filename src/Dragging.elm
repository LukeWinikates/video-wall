module Dragging exposing (..)

import Mouse


-- needs: a function for assembling Msg from Move/End signals


type DragEventType
    = Start
    | Move
    | End


type DragEvent
    = DragEvent DragEventType Mouse.Position


type alias Drag =
    { current : { x : Int, y : Int }
    , start : { x : Int, y : Int }
    }


type alias DragModelState =
    { drag : Maybe Drag
    , position : { x : Int, y : Int }
    }



--type alias Options =
--    { updatePosition: ({x:Int, y: Int} -> a)
--     , updateDragging: ({ start: Mouse.Position, current: Mouse.Position } -> b )
--     }
--
--
--init : Options ->
--
--dragSubscriptions : a -> List (Sub b)
--dragSubscriptions model =
--    [ Mouse.moves (Move), Mouse.ups (End) ]


calcPosition : DragModelState -> Mouse.Position -> DragModelState
calcPosition model position =
    (Maybe.map
        (\drag ->
            { model
                | position =
                    { y = position.y + drag.current.y - drag.start.y
                    , x = position.x + drag.current.x - drag.start.x
                    }
                , drag = Just { drag | current = position }
            }
        )
        model.drag
    )
        |> Maybe.withDefault model


handle : DragEvent -> DragModelState -> DragModelState
handle (DragEvent typ position) state =
    case typ of
        Start ->
            { drag = Just { current = position, start = position }
            , position = state.position
            }

        Move ->
            calcPosition state position

        End ->
            { drag = Nothing, position = position }
