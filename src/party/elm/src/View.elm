module View exposing (..)

import Html exposing (Html, main_, button, img, div, p, span, hr, h2, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


root : Model -> Html Msg
root model =
    main_ []
        [ h2 [] [ text "chancesnow.me/party" ]
        , div
            [ Html.Attributes.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                , ( "justify-content", "center" )
                ]
            ]
            [ img
                [ Html.Attributes.src "/assets/images/party/Party-logo-invert.png"
                , Html.Attributes.alt "Party"
                , Html.Attributes.style
                    [ ( "max-width", "454px" )
                    , ( "margin", "0" )
                    ]
                ]
                []
            , p [ Html.Attributes.style [ ( "margin-top", "0" ) ] ] [ text "Prototype" ]
            ]
        , div []
            [ hr [] []
            , Html.a
                [ href "/party/old.html"
                , Html.Attributes.style
                    [ ( "cursor", "pointer" )
                    , ( "font-size", "14pt" )
                    , ( "color", "white" )
                    ]
                ]
                [ text "Prettier Mockup" ]
            ]
        ]
