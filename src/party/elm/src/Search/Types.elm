module Search.Types exposing (..)


type alias Model =
    { query : String
    }


initialState : Model
initialState =
    { query = "" }


type Msg
    = ChangeQuery String
    | Search
