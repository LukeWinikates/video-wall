module Movie.Parser exposing (..)

import Geometry exposing (..)
import Combine exposing (..)
import Combine.Num exposing (int)


type alias MovieDefinition =
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


movie : Parser s MovieDefinition
movie =
    (\orientation scale top left movieId ->
        { orientation = orientation
        , scale = scale
        , top = top
        , left = left
        , movieId = movieId
        }
    )
        <$> orientation
        <*> (dash *> scale)
        <*> (dash *> int)
        <*> (dash *> int)
        <*> (dash *> (toString <$> int) <* end)


parseMovie : String -> Result String MovieDefinition
parseMovie input =
    case parse movie input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join " or " errors)
