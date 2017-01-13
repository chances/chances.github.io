module Main exposing (..)

import Html exposing (program)
import Model exposing (Model, Msg)
import State exposing (..)
import View exposing (..)


-- "circuithub/elm-list-extra": "3.0.0 <= v < 4.0.0",


main : Program Never Model Msg
main =
    program
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
