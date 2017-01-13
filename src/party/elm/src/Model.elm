module Model exposing (..)

import Time.DateTime exposing (DateTime)
import Token.Types as Token
import Ping.Types as Ping
import Search.Types as Search


type alias Model =
    { tvMode : Bool
    , ping : Ping.Model
    , token : Token.Model
    , search : Search.Model
    }


initialState : Model
initialState =
    { tvMode = False
    , ping = Ping.initialState
    , token = Token.initialState
    , search = Search.initialState
    }


type Msg
    = GetTimeAndThen (DateTime -> Msg)
    | Ping Ping.Msg
    | Token Token.Msg
    | Search Search.Msg
