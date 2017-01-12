module Main exposing (..)

import Debug
import Html exposing (Html, main_, button, img, div, p, span, hr, h2, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Time
import HttpBuilder exposing (withCredentials, withExpect, withTimeout, send)
import Types.Envelope exposing (Envelope, isErrorEnvelope)
import Types.TokenResponse exposing (TokenResponse, tokenResponseDecoder)
import Types.PingResponse exposing (PingResponse, pingResponseDecoder)


-- "circuithub/elm-list-extra": "3.0.0 <= v < 4.0.0",


type alias Model =
    { tvMode : Bool
    , maybePingResponse : Maybe PingResponse
    , maybeToken : Maybe TokenResponse
    , searchQuery : String
    }


initialState : Model
initialState =
    Model False Nothing Nothing ""


type Msg
    = Pong (Maybe PingResponse)
    | UpdateToken (Maybe TokenResponse)


init : ( Model, Cmd Msg )
init =
    ( initialState, pingApi )


pingApi : Cmd Msg
pingApi =
    let
        complete : Result Http.Error (Envelope PingResponse) -> Msg
        complete result =
            case result of
                Ok pingResponseEnvelope ->
                    case isErrorEnvelope pingResponseEnvelope of
                        True ->
                            Pong Nothing

                        -- TODO: Handle error envelope
                        False ->
                            Pong pingResponseEnvelope.data

                Err _ ->
                    Pong Nothing

        -- TODO: Handle Http error
    in
        HttpBuilder.get "http://app.local:3000/auth/ping"
            |> withCredentials
            |> withExpect (Http.expectJson pingResponseDecoder)
            |> withTimeout (10 * Time.second)
            |> send complete


getToken : Maybe PingResponse -> Cmd Msg
getToken maybePingResponse =
    case maybePingResponse of
        Just pingResponse ->
            let
                complete : Result Http.Error TokenResponse -> Msg
                complete result =
                    case result of
                        Ok tokenResponse ->
                            UpdateToken (Just tokenResponse)

                        Err _ ->
                            UpdateToken Nothing

                url =
                    "http://app.local:3000/auth/token"

                hostedUrl =
                    "https://party.chancesnow.me/auth/token"
            in
                HttpBuilder.get url
                    |> withCredentials
                    |> withExpect (Http.expectJson tokenResponseDecoder)
                    |> withTimeout (10 * Time.second)
                    |> send complete

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pong maybePingResponse ->
            ( Model
                model.tvMode
                (Debug.log "pong: " maybePingResponse)
                model.maybeToken
                model.searchQuery
            , getToken maybePingResponse
            )

        UpdateToken maybeToken ->
            ( Model
                model.tvMode
                model.maybePingResponse
                (Debug.log "token" maybeToken)
                model.searchQuery
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


renderToken : Maybe TokenResponse -> List (Html msg)
renderToken maybeToken =
    [ text "Token: "
    , case maybeToken of
        Just token ->
            text token.accessToken

        Nothing ->
            text "None"
    ]


view : Model -> Html Msg
view model =
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
