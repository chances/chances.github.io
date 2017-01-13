module Token.Rest exposing (getToken)

import Http
import HttpBuilder exposing (withCredentials, withExpect, withTimeout, send)
import Time
import Model
import Token.Types exposing (..)
import Ping.Types exposing (PingResponse)


getToken : Maybe PingResponse -> Cmd Msg
getToken maybePingResponse =
    case maybePingResponse of
        Just pingResponse ->
            let
                complete : Result Http.Error Token -> Msg
                complete result =
                    case result of
                        Ok tokenResponse ->
                            UpdateToken (Just tokenResponse)

                        Err _ ->
                            UpdateToken Nothing

                url =
                    "http://app.local:3000/auth/token"

                hostedUrl =
                    "https://party.chancesnow.me/auth/token"
            in
                HttpBuilder.get url
                    |> withCredentials
                    |> withExpect (Http.expectJson tokenDecoder)
                    |> withTimeout (10 * Time.second)
                    |> send complete

        Nothing ->
            Cmd.none
