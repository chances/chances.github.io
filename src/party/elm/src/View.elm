module View exposing (root, tvMode)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


root : Model -> Html Msg
root model =
    main_ []
        [ h2 [ tvMode ] [ text "chancesnow.me/party" ]
        , prototypeHeader
        , div []
            [ hr [] []
            , a
                [ href "/party/old.html"
                , style
                    [ ( "cursor", "pointer" )
                    , ( "font-size", "14pt" )
                    , ( "color", "white" )
                    ]
                ]
                [ text "Prettier Mockup" ]
            ]
        ]


tvMode : Attribute Msg
tvMode =
    class "tv-mode"


prototypeHeader : Html Msg
prototypeHeader =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ img
            [ src "/assets/images/party/Party-logo-invert.png"
            , alt "Party"
            , style
                [ ( "max-width", "454px" )
                , ( "margin", "0" )
                ]
            ]
            []
        , p [ style [ ( "margin-top", "0" ) ] ] [ text "Prototype" ]
        ]
