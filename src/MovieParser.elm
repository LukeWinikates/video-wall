module MovieParser exposing (..)

import Geometry exposing (..)
import Movie exposing (..)
import Combine exposing (..)
import Combine.Num exposing (int)


dash =
    string "-"


type alias MovieDefinition =
    { orientation : Orientation
    , top : Int
    , bottom : Int
    , left : Int
    , right : Int
    , movieId : String
    }


orientation : Parser s Orientation
orientation =
    or (string "V" $> Vertical)
        (string "H" $> Horizontal)


makeRecord : Orientation -> Int -> Int -> Int -> Int -> String -> MovieDefinition
makeRecord orientation top left bottom right movieId =
    { orientation = orientation
    , top = top
    , bottom = bottom
    , left = left
    , right = right
    , movieId = movieId
    }


movie : Parser s MovieDefinition
movie =
    makeRecord
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
