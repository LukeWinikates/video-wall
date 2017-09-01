module App.Routing exposing (..)

import Movie
import Navigation exposing (..)
import UrlParser exposing (..)
import Model exposing (Model, empty, gridItemsFromCommaSeparatedList)
import App.Msg exposing (Msg(..))
import Model.Serialize exposing (toUrl)


type Route
    = AppRoute String String


program =
    Navigation.program


route : Parser (Route -> a) a
route =
    UrlParser.map AppRoute (string </> string)


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    (parseHash route location)
        |> Maybe.map modelFrom
        |> Maybe.map (flip (,) Cmd.none)
        |> Maybe.withDefault ( Model.default, Model.default |> modelToUrlCmd )


modelFrom : Route -> Model
modelFrom (AppRoute collectionId itemsString) =
    Maybe.withDefault Model.empty <|
        (Maybe.map
            (\collection ->
                { empty
                    | movies = itemsString |> gridItemsFromCommaSeparatedList collection
                    , collection = collection
                }
            )
            (Movie.fromCollectionId
                collectionId
            )
        )


modelToUrlCmd : Model -> Cmd Msg
modelToUrlCmd model =
    model |> toUrl |> Navigation.modifyUrl
