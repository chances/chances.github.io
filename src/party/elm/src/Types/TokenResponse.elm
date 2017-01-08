module TokenResponse exposing (TokenResponse, tokenResponseDecoder)

import Json.Decode exposing (Decoder, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Time.DateTime exposing (DateTime, fromISO8601)


type alias TokenResponse =
    { access_token : String
    , expiry_date : DateTime
    , scope : String
    }


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
