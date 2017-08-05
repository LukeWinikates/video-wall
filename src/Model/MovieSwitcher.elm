module Model.MovieSwitcher exposing (replaceMovies)

import Geometry exposing (Orientation(Horizontal, Vertical))
import Model exposing (GridContent(Content, Initial, Picking), GridItem)
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
            List.filterMap orientationFrom gridItems
    in
        pickSubstitutableMovies { horizontals = horizontals, verticals = verticals, remaining = remaining, populated = [] }
            |> injectMoviesInto gridItems


orientationFrom : GridItem -> Maybe Orientation
orientationFrom gridItem =
    case gridItem.content of
        Content o _ _ _ ->
            Just o

        _ ->
            Nothing


swapOrUnsetMovieContent : GridItem -> Maybe Movie -> GridItem
swapOrUnsetMovieContent item maybeMovie =
    case maybeMovie of
        Just movie ->
            Model.Mutate.setMovie movie item

        Nothing ->
            Model.Mutate.content (Initial Nothing) item


injectMoviesInto : List GridItem -> List (Maybe Movie) -> List GridItem
injectMoviesInto gridItems maybeMovies =
    List.Extra.andMap maybeMovies (List.map swapOrUnsetMovieContent gridItems)


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
