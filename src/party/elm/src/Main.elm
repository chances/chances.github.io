module Main exposing (..)

import Html exposing (Html, body, button, div, p, span, hr, h2, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Time
import HttpBuilder exposing (withExpect, withTimeout, send)
import Types.TokenResponse exposing (TokenResponse, tokenResponseDecoder)


-- "circuithub/elm-list-extra": "3.0.0 <= v < 4.0.0",


type alias Model =
    { tvMode : Bool
    , maybeToken : Maybe TokenResponse
    , searchQuery : String
    }


initialState : Model
initialState =
    Model False Nothing ""


type Msg
    = Reset
    | GetToken
    | UpdateToken (Maybe TokenResponse)


init : ( Model, Cmd Msg )
init =
    ( initialState, Cmd.none )


getToken : Cmd Msg
getToken =
    let
        complete : Result Http.Error TokenResponse -> Msg
        complete result =
            case result of
                Ok tokenResponse ->
                    UpdateToken (Just tokenResponse)

                Err _ ->
                    UpdateToken Nothing

        url =
            "http://localhost:3000/auth/token"

        hostedUrl =
            "https://party.chancesnow.me/auth/token"
    in
        HttpBuilder.get url
            |> withExpect (Http.expectJson tokenResponseDecoder)
            |> withTimeout (10 * Time.second)
            |> send complete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init

        GetToken ->
            ( model, getToken )

        UpdateToken maybeToken ->
            ( Model model.tvMode maybeToken ""
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
    body []
        [ h2 [] [ text "chancesnow.me/party" ]
        , div []
            [ p [] (renderToken model.maybeToken)
            , button [ onClick GetToken ] [ text "Get Token" ]
            , hr [] []
            , p [] [ text (toString model.maybeToken) ]
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
