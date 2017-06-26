module MovieParserTest exposing (..)

import Test exposing (..)
import Expect
import MovieParser exposing (..)
import Geometry exposing (..)
import Movie exposing (..)


-- V-2-1-8-5-6259,H-1-5-6-12-6219,H-6-5-10-12-6259,V-2-12-8-16-6260
-- TODO: change to 2,1-8x5


all : Test
all =
    test "parseMovie" <|
        \() ->
            Expect.equal
                (Ok
                    { orientation = Vertical
                    , top = 2
                    , left = 1
                    , height = 8
                    , width = 5
                    , movieId = "6259"
                    }
                )
                (MovieParser.parseMovie "V-2-1-8-5-6259")
