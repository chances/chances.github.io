module Search.State exposing (init, update, subscriptions)

import Debug
import Search.Types exposing (..)
import Spotify exposing (searchTracks)
import Model as Model


init : Cmd Model.Msg
init =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search maybeQuery ->
            ( model
            , searchTracks maybeQuery
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
