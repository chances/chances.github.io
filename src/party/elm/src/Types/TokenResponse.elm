module Types.TokenResponse exposing (..)

import Json.Decode exposing (Decoder, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Time.DateTime exposing (DateTime, fromISO8601)


type alias TokenResponse =
    { accessToken : String
    , expiryDate : DateTime
    , scope : String
    }


tokenExpired : TokenResponse -> DateTime -> Bool
tokenExpired tokenResponse currentTime =
    case Time.DateTime.compare tokenResponse.expiryDate currentTime of
        GT ->
            False

        _ ->
            True


tokenResponseDecoder : Decoder TokenResponse
tokenResponseDecoder =
    let
        toDecoder : String -> String -> String -> Decoder TokenResponse
        toDecoder token expiryDateString scope =
            case fromISO8601 expiryDateString of
                Ok expiryDate ->
                    succeed (TokenResponse token expiryDate scope)

                Err err ->
                    fail err
    in
        decode toDecoder
            |> required "access_token" string
            |> required "expiry_date" string
            |> required "scope" string
            |> resolve
