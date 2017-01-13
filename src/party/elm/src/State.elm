module State exposing (init, update, subscriptions)

import Debug
import Task
import Time
import Time.DateTime exposing (fromTimestamp)
import Token.Rest as Token
import Token.State as Token
import Token.Types as Token
import Ping.Rest as Ping
import Ping.Types as Ping
import Model exposing (..)


init : ( Model, Cmd Msg )
init =
    ( initialState
    , Cmd.batch [ Token.init ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTimeAndThen successHandler ->
            ( model
            , (Task.perform
                (\time -> successHandler (fromTimestamp time))
                Time.now
              )
            )

        Ping pingMsg ->
            case pingMsg of
                Ping.Pong maybePingResponse ->
                    ( { model | ping = Ping.Model (Debug.log "pong" maybePingResponse) }
                    , getToken maybePingResponse
                    )

        Token tokenMsg ->
            let
                ( tokenModel, command ) =
                    Token.update tokenMsg model.token
            in
                ( { model | token = tokenModel }
                , Cmd.map Token command
                )


getToken : Maybe Ping.PingResponse -> Cmd Msg
getToken maybePingResponse =
    Cmd.map Token (Token.getToken maybePingResponse)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Token (Token.subscriptions model.token)
        ]
