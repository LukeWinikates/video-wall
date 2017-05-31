module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    {  }


init : ( Model, Cmd Msg )
init =
    ( {
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


type Msg = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model = (model, Cmd.none)


view : Model -> Html Msg
view model =
    div
        [
        ]
        []
