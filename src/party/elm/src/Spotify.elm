port module Spotify exposing (..)

import Json.Encode


port updateToken : Maybe String -> Cmd msg


port searchTracks : String -> Cmd msg


port tracksResults : (Json.Encode.Value -> msg) -> Sub msg
