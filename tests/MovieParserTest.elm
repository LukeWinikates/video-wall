module MovieParserTest exposing (..)

import Test exposing (..)
import Expect
import Movie.Parser exposing (..)
import Geometry exposing (..)
import Movie exposing (..)


all : Test
all =
    test "parseMovie" <|
        \() ->
            Expect.equal
                (Ok
                    { orientation = Vertical
                    , top = 2
                    , left = 1
                    , scale = Large
                    , movieId = "6259"
                    }
                )
                (Movie.Parser.parseMovie "V-L-2-1-6259")
