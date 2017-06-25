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



-- TODO: I don't really like the makeRecord function, but I'm not sure how else to express it
-- I kind of want to be able to interleave the parser-isms and the record construction,
-- like  {
--          orientation = orientation
--          top = - int
--          left = - int
--          bottom = - int
--          right = - int
--
--        }
-- or  {
--          map \v -> { orientation = v } orientation
--       <*> \x v ->  {x | top = v} (dash *> int)
--       <*> \x v ->  {x | left = v} (dash *> int)
--       <*> \x v ->  {x | bottom = v} (dash *> int)
--       <*> \x v ->  {x | right = v} (dash *> int)
--       <*> \x v ->  {x | movieId = v} (dash *> int)
--          top = - int
--          left = - int
--          bottom = - int
--          right = - int
--
--        }
-- but I don't know if the syntax exists


movie : Parser s MovieDefinition
movie =
    (\orientation top left bottom right movieId ->
        { orientation = orientation
        , top = top
        , left = left
        , bottom = bottom
        , right = right
        , movieId = movieId
        }
    )
        <$> orientation
        <*> (dash *> int)
        <*> (dash *> int)
        <*> (dash *> int)
        <*> (dash *> int)
        <*> (dash *> (toString <$> int) <* end)



-- here's the best thing I could make work (and I'm very proud!)
-- but of course, it's a bad solution because to get the compiler to agree
-- I had to put all of the fields into the initial record
-- which means they start off with "null" values, basically...
-- which is not what I wanted.
-- although I think anyone else would say that the original version is better,
-- I do like that this visually aligns the parsers with the field names that they're responsible for producing
--


movie2 : Parser s MovieDefinition
movie2 =
    ((\v -> { orientation = v, top = 0, left = 0, right = 0, bottom = 0, movieId = "" }) <$> orientation)
        |> andThen (\x -> ((\v -> { x | top = v }) <$> (dash *> int)))
        |> andThen (\x -> ((\v -> { x | left = v }) <$> (dash *> int)))
        |> andThen (\x -> ((\v -> { x | bottom = v }) <$> (dash *> int)))
        |> andThen (\x -> ((\v -> { x | right = v }) <$> (dash *> int)))
        |> andThen (\x -> ((\v -> { x | movieId = v }) <$> (dash *> (toString <$> int) <* end)))



-- here's an approach where the parsers themselves are a record
-- It kind of looks cool, but I don't know if it's better than the improved version of the original movie parser
-- I don't know that the indirection is helpful. I like that the sub-parsers now are associated with their names
-- on the other hand, maybe it's harder to see the connection back to the actual string format
movie3 : Parser s MovieDefinition
movie3 =
    let
        componentParsers =
            { orientation = orientation
            , top = dash *> int
            , left = dash *> int
            , bottom = dash *> int
            , right = dash *> int
            , movieId = dash *> (toString <$> int) <* end
            }
    in
        (\orientation top left bottom right movieId ->
            { orientation = orientation
            , top = top
            , left = left
            , bottom = bottom
            , right = right
            , movieId = movieId
            }
        )
            <$> componentParsers.orientation
            <*> componentParsers.top
            <*> componentParsers.left
            <*> componentParsers.bottom
            <*> componentParsers.right
            <*> componentParsers.movieId


parseMovie : String -> Result String MovieDefinition
parseMovie input =
    case parse movie input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join " or " errors)
