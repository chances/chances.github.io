module State exposing (init, update, subscriptions)

import Debug
import Task
import Time
import Time.DateTime exposing (fromTimestamp)
import Token.Rest as Token
import Token.Types as Token
import Ping.Rest as Ping
import Ping.Types as Ping
import Model exposing (..)


init : ( Model, Cmd Msg )
init =
    ( initialState, ping )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTimeAndThen successHandler ->
            ( model, (Task.perform (\time -> successHandler (fromTimestamp time)) Time.now) )

        Ping pingMsg ->
            case pingMsg of
                Ping.Pong maybePingResponse ->
                    ( { model | ping = Ping.Model (Debug.log "pong" maybePingResponse) }
                    , getToken maybePingResponse
                    )

        Token tokenMsg ->
            case tokenMsg of
                Token.UpdateToken maybeToken ->
                    ( { model | token = Token.Model (Debug.log "token" maybeToken) }
                    , Cmd.none
                    )


ping : Cmd Msg
ping =
    Cmd.map Ping Ping.ping


getToken : Maybe Ping.PingResponse -> Cmd Msg
getToken maybePingResponse =
    Cmd.map Token (Token.getToken maybePingResponse)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
