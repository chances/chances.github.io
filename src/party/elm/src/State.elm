module State exposing (init, update, subscriptions)

import Task
import Time
import Time.DateTime exposing (fromTimestamp)
import Ping.Rest as Ping
import Ping.Types as Ping
import Token.Rest as Token
import Token.State as Token
import Token.Types as Token
import Search.State as Search
import Search.Types as Search
import Model exposing (..)


init : ( Model, Cmd Msg )
init =
    ( initialState
    , Cmd.batch [ Token.init ]
    )



-- Look into http://package.elm-lang.org/packages/Fresheyeball/elm-return


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
                    ( { model | ping = Ping.Model maybePingResponse }
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

        Search searchMsg ->
            let
                ( searchModel, command ) =
                    Search.update searchMsg model.search
            in
                ( { model | search = searchModel }
                , Cmd.map Search command
                )


getToken : Maybe Ping.PingResponse -> Cmd Msg
getToken maybePingResponse =
    Cmd.map Token (Token.getToken maybePingResponse)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Token (Token.subscriptions model.token)
        , Sub.map Search (Search.subscriptions model.search)
        ]
