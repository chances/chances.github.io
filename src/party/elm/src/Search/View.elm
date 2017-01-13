module Search.View exposing (root)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Search.Types exposing (..)
import Model as Model


root : Model -> Html Model.Msg
root model =
    div []
        [ input
            [ placeholder "Search for a song"
            , onInput (\str -> Model.Search (ChangeQuery str))
            ]
            []
        , renderSearchButton model "Search"
        ]


renderSearchButton : Model -> String -> Html Model.Msg
renderSearchButton model buttonLabel =
    button
        [ onClick (Model.Search Search) ]
        [ case String.isEmpty model.query of
            False ->
                text ("Search for \"" ++ model.query ++ "\"")

            True ->
                text ("Search for nothing")
        ]
