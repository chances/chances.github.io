module Token.State exposing (init, update, subscriptions)

import Debug
import Task
import Time
import Time.DateTime exposing (fromTimestamp)
import Ping.Rest as Ping
import Token.Rest exposing (..)
import Token.Types exposing (..)
import Model as Model


init : Cmd Model.Msg
init =
    ping


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateToken maybeToken ->
            ( Model maybeToken False
            , Task.perform checkToken Time.now
            )

        CheckToken currentTime ->
            case model.maybeToken of
                Just token ->
                    if tokenExpired token currentTime then
                        ( { model | maybeToken = Nothing, expired = True }
                        , Cmd.none
                          -- TODO: Refresh the token somehow
                        )
                    else
                        ( { model | expired = False }
                        , Cmd.none
                        )

                Nothing ->
                    ( { model | expired = True }
                    , Cmd.none
                    )


ping : Cmd Model.Msg
ping =
    Cmd.map Model.Ping Ping.ping


checkToken : Time.Time -> Msg
checkToken time =
    CheckToken (fromTimestamp time)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.maybeToken of
        Just _ ->
            Time.every (500 * Time.millisecond) checkToken

        Nothing ->
            Sub.none
