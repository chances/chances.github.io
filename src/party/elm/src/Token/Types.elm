module Token.Types exposing (..)

import Json.Decode exposing (Decoder, string, succeed, fail)
import Json.Decode.Pipeline exposing (decode, required, resolve)
import Time.DateTime exposing (DateTime, fromISO8601)


type alias Token =
    { accessToken : String
    , expiryDate : DateTime
    , scope : String
    }


tokenExpired : Token -> DateTime -> Bool
tokenExpired tokenResponse currentTime =
    case Time.DateTime.compare tokenResponse.expiryDate currentTime of
        GT ->
            False

        _ ->
            True


tokenDecoder : Decoder Token
tokenDecoder =
    let
        toDecoder : String -> String -> String -> Decoder Token
        toDecoder token expiryDateString scope =
            case fromISO8601 expiryDateString of
                Ok expiryDate ->
                    succeed (Token token expiryDate scope)

                Err err ->
                    fail err
    in
        decode toDecoder
            |> required "access_token" string
            |> required "expiry_date" string
            |> required "scope" string
            |> resolve


type alias Model =
    { maybeToken : Maybe Token
    }


initialState : Model
initialState =
    Model Nothing


type Msg
    = UpdateToken (Maybe Token)
