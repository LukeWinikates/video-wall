module Model.Parser exposing (..)

import Geometry exposing (..)
import Combine exposing (..)
import Combine.Num exposing (int)


type alias ItemDescription =
    { orientation : Orientation
    , top : Int
    , left : Int
    , scale : Scale
    , movieId : String
    }


orientation : Parser s Orientation
orientation =
    or (string "V" $> Vertical)
        (string "H" $> Horizontal)


scale : Parser s Scale
scale =
    choice
        [ (string "S" $> Small)
        , (string "M" $> Medium)
        , (string "L" $> Large)
        ]


dash =
    string "-"


movie : Parser s ItemDescription
movie =
    (\top left orientation scale movieId ->
        { orientation = orientation
        , scale = scale
        , top = top
        , left = left
        , movieId = movieId
        }
    )
        <$> int
        <*> (dash *> int)
        <*> (dash *> orientation)
        <*> (dash *> scale)
        <*> (dash *> (toString <$> int) <* end)


parseItem : String -> Result String ItemDescription
parseItem input =
    case parse movie input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join " or " errors)
