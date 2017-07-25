module Movie.Parser exposing (..)

import Geometry exposing (..)
import Combine exposing (..)
import Combine.Num exposing (int)


type alias MovieDefinition =
    { orientation : Orientation
    , top : Int
    , height : Int
    , left : Int
    , width : Int
    , movieId : String
    }


orientation : Parser s Orientation
orientation =
    or (string "V" $> Vertical)
        (string "H" $> Horizontal)


dash =
    string "-"


movie : Parser s MovieDefinition
movie =
    (\orientation top left height width movieId ->
        { orientation = orientation
        , top = top
        , left = left
        , height = height
        , width = width
        , movieId = movieId
        }
    )
        <$> orientation
        <*> (dash *> int)
        <*> (dash *> int)
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
