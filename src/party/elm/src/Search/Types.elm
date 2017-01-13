module Search.Types exposing (..)


type alias Model =
    { query : Maybe String
    }


initialState : Model
initialState =
    { query = Nothing }


type Msg
    = Search (Maybe String)
