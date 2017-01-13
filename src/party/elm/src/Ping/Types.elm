module Ping.Types exposing (..)

import Json.Decode exposing (Decoder, field, string, bool)
import Json.Decode.Pipeline exposing (decode, required)
import Types.Envelope exposing (Envelope, envelopeDecoder)


type alias PingResponse =
    { apiStatus : ApiStatus
    , message : String
    }


type alias ApiStatus =
    { available : Bool
    }


pingResponseDecoder : Decoder (Envelope PingResponse)
pingResponseDecoder =
    let
        decodePingResponse =
            decode PingResponse
                |> required "api_status" apiStatusDecoder
                |> required "message" string
    in
        envelopeDecoder decodePingResponse


apiStatusDecoder : Decoder ApiStatus
apiStatusDecoder =
    decode ApiStatus
        |> required "available" bool


type alias Model =
    { maybePingResponse : Maybe PingResponse
    }


initialState : Model
initialState =
    Model Nothing


type Msg
    = Pong (Maybe PingResponse)
