import Json.Decode exposing (int, string, float, customDecoder, Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Time.DateTime exposing (DateTime, fromISO8601)

-- import Types.Time exposing (Time)

type alias TokenResponse =
    { access_token : String
    , expiry_date : DateTime
    , scope : String
    }

decodeTokenResponse : Decoder TokenResponse
decodeTokenResponse =
    decode TokenResponse
        |> required "access_token" string
        |> required "expiry_date" decodeDateTime
        |> required "scope" string

-- string : Decoder a -> Decoder (a -> b) -> Decoder b

decodeDateTime : Decoder a -> Decoder (a -> b) -> Decoder DateTime
decodeDateTime valDecoder _ = customDecoder valDecoder fromISO8601
