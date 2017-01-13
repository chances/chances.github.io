module Search.State exposing (init, update, subscriptions)

import Search.Types exposing (..)
import Spotify
import Model as Model


init : Cmd Model.Msg
init =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( model
            , Spotify.searchTracks model.query
            )

        ChangeQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
