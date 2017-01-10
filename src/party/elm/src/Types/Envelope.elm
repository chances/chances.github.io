module Types.Envelope exposing (Envelope, envelopeDecoder, isErrorEnvelope)

import Json.Decode exposing (Decoder, string, bool, nullable)
import Json.Decode.Pipeline exposing (decode, optional)


type alias Envelope a =
    { data : Maybe a
    , error : Maybe String
    , extra : Maybe String
    }


isErrorEnvelope : Envelope a -> Bool
isErrorEnvelope aEnvelope =
    case aEnvelope.error of
        Nothing ->
            False

        Just _ ->
            True


envelopeDecoder : Decoder a -> Decoder (Envelope a)
envelopeDecoder dataDecoder =
    decode Envelope
        |> optional "data" (nullable dataDecoder) Nothing
        |> optional "error" (nullable string) Nothing
        |> optional "extra" (nullable string) Nothing
