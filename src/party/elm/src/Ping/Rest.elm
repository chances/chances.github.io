module Ping.Rest exposing (ping)

import Http
import HttpBuilder exposing (withCredentials, withExpect, withTimeout, send)
import Time
import Types.Envelope exposing (Envelope, isErrorEnvelope)
import Ping.Types exposing (..)


ping : Cmd Msg
ping =
    let
        complete : Result Http.Error (Envelope PingResponse) -> Msg
        complete result =
            case result of
                Ok pingResponseEnvelope ->
                    case isErrorEnvelope pingResponseEnvelope of
                        True ->
                            Pong Nothing

                        -- TODO: Handle error envelope
                        False ->
                            Pong pingResponseEnvelope.data

                Err _ ->
                    Pong Nothing

        -- TODO: Handle Http error
    in
        HttpBuilder.get "http://app.local:3000/auth/ping"
            |> withCredentials
            |> withExpect (Http.expectJson pingResponseDecoder)
            |> withTimeout (10 * Time.second)
            |> send complete
