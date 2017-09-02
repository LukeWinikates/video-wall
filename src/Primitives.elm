module Primitives exposing (resultToMaybe, orMaybe)


resultToMaybe : Result a b -> Maybe b
resultToMaybe =
    Result.map Just >> Result.withDefault Nothing


orMaybe : Maybe a -> Maybe a -> Maybe a
orMaybe firstOption secondOption =
    List.filterMap identity [ firstOption, secondOption ]
        |> List.head
