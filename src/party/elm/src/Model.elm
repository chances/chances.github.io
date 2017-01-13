module Model exposing (..)

import Time.DateTime exposing (DateTime)
import Token.Types as Token
import Ping.Types as Ping


type alias Model =
    { tvMode : Bool
    , ping : Ping.Model
    , token : Token.Model
    , searchQuery : String
    }


initialState : Model
initialState =
    Model False Ping.initialState Token.initialState ""


type Msg
    = GetTimeAndThen (DateTime -> Msg)
    | Ping Ping.Msg
    | Token Token.Msg
