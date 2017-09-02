module Model.MovieSwitcher exposing (replaceMovies)

import Geometry exposing (Orientation(Horizontal, Vertical))
import Model exposing (GridItem)
import Movie exposing (Movie, MovieCollection)
import List.Extra exposing (andMap)
import List exposing (map)
import Model.Mutate


replaceMovies : List GridItem -> MovieCollection -> List GridItem
replaceMovies gridItems collection =
    let
        verticals =
            Movie.byOrientation collection Vertical

        horizontals =
            Movie.byOrientation collection Horizontal

        remaining =
            List.map .orientation gridItems
    in
        pickSubstitutableMovies { horizontals = horizontals, verticals = verticals, remaining = remaining, populated = [] }
            |> injectMoviesInto gridItems


swapOrUnsetMovieContent : GridItem -> Movie -> GridItem
swapOrUnsetMovieContent item movie =
    Model.Mutate.setMovie movie item


injectMoviesInto : List GridItem -> List (Maybe Movie) -> List GridItem
injectMoviesInto gridItems maybeMovies =
    List.Extra.andMap (List.filterMap identity maybeMovies) (List.map swapOrUnsetMovieContent gridItems)


type alias MovieSubstitutionState =
    { horizontals : List Movie
    , verticals : List Movie
    , remaining : List Orientation
    , populated : List (Maybe Movie)
    }


pickSubstitutableMovies : MovieSubstitutionState -> List (Maybe Movie)
pickSubstitutableMovies state =
    case state.remaining of
        [] ->
            state.populated

        next :: rest ->
            case next of
                Vertical ->
                    pickSubstitutableMovies
                        { state
                            | verticals = List.drop 1 state.verticals
                            , remaining = rest
                            , populated = state.populated ++ [ List.head state.verticals ]
                        }

                Horizontal ->
                    pickSubstitutableMovies
                        { state
                            | horizontals = List.drop 1 state.horizontals
                            , remaining = rest
                            , populated = state.populated ++ [ List.head state.horizontals ]
                        }
