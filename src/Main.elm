port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (Value)
import Query exposing (..)
import Task
import Time
import Url


-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- PORTS


port signIn : () -> Cmd msg


port signOut : () -> Cmd msg


port sendQuery : Query.Query -> Cmd msg


port queryResponseReceiver : (Json.Decode.Value -> msg) -> Sub msg



-- MODEL


type alias Flags =
    { user : Maybe User
    }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , flags : Flags
    , zone : Time.Zone
    , time : Time.Posix
    }


type alias User =
    { uid : String
    }


type alias Query =
    Int


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , flags = flags
      , zone = Time.utc
      , time = Time.millisToPosix 0
      }
    , initCmd flags
    )


initCmd : Flags -> Cmd Msg
initCmd flags =
    case flags.user of
        Nothing ->
            Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                ]

        Just user ->
            Cmd.batch
                [ Task.perform AdjustTimeZone Time.here
                , sendQuery nodeQuery
                ]


nodeQuery : Query.Query
nodeQuery =
    { id = "nodes"
    , path = [ "nodes" ]
    , limit = 10
    }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AdjustTimeZone Time.Zone
    | SignIn
    | SignOut
    | QueryResponseReceived Json.Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        SignIn ->
            ( model
            , signIn ()
            )

        SignOut ->
            ( model
            , signOut ()
            )

        QueryResponseReceived response ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    queryResponseReceiver QueryResponseReceived



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.flags.user of
        Nothing ->
            loginView model

        Just user ->
            mainView model


loginView : Model -> Browser.Document Msg
loginView model =
    { title = "Ficus"
    , body =
        [ Element.layout [] <|
            Element.column []
                [ Element.el [ alignRight ] signInButton ]
        ]
    }


mainView : Model -> Browser.Document Msg
mainView model =
    { title = "Ficus"
    , body =
        [ Element.layout [] <|
            Element.column []
                [ Element.el [ alignRight ] signOutButton ]
        ]
    }


signInButton =
    Input.button
        []
        { label = Element.text "Sign In"
        , onPress = Just SignIn
        }


signOutButton =
    Input.button
        []
        { label = Element.text "Sign Out"
        , onPress = Just SignOut
        }
