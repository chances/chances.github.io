module Search.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Search.Types exposing (..)
import Model as Model


root : Model -> Html Model.Msg
root model =
    div [] [ renderSearchButton model "Search" ]


renderSearchButton : Model -> String -> Html Model.Msg
renderSearchButton model buttonLabel =
    button
        [ onClick (Model.Search (Search model.query)) ]
        [ case model.query of
            Just query ->
                text ("Search for \"" ++ query ++ "\"")

            Nothing ->
                text ("Search for nothing")
        ]
