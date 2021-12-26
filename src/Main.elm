port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Query exposing (..)
import Result.Extra as Result
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
    , err : Maybe String
    , nodes : List Node
    }


type alias User =
    { uid : String
    }


type alias Node =
    { id : String
    , name : String
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
      , err = Nothing
      , nodes = []
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
    | ErrorParsingResponse String
    | QueryResponseReceived Json.Decode.Value
    | NodesReceived (List Node)


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
            update (decodeAndExtract response) model

        ErrorParsingResponse err ->
            ( { model | err = Just err }
            , Cmd.none
            )

        NodesReceived nodes ->
            ( { model | nodes = nodes }
            , Cmd.none
            )


decodeAndExtract : Json.Decode.Value -> Msg
decodeAndExtract response =
    Json.Decode.decodeValue queryResponseDecoder response
        |> Result.extract parseErrorToMessage


parseErrorToMessage : Json.Decode.Error -> Msg
parseErrorToMessage err =
    ErrorParsingResponse (Json.Decode.errorToString err)


queryResponseDecoder : Json.Decode.Decoder Msg
queryResponseDecoder =
    Query.snapshotDecoder |> Json.Decode.andThen snapshotToMessageDecoder


snapshotToMessageDecoder : Query.Snapshot -> Json.Decode.Decoder Msg
snapshotToMessageDecoder snapshot =
    case snapshot.id of
        "nodes" ->
            nodeDecoder
                |> Json.Decode.field "data"
                |> Json.Decode.list
                |> Json.Decode.field "docs"
                |> Json.Decode.map NodesReceived

        _ ->
            Json.Decode.fail ("unknown query id: " ++ snapshot.id)


nodeDecoder : Json.Decode.Decoder Node
nodeDecoder =
    Json.Decode.map2 Node
        (field "id" Json.Decode.string)
        (field "name" Json.Decode.string)



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
                [ Element.el [ alignRight ] signOutButton
                , errView model.err
                , nodesView model.nodes
                ]
        ]
    }


errView : Maybe String -> Element Msg
errView maybeErr =
    case maybeErr of
        Nothing ->
            Element.text "no error"

        Just err ->
            Element.text err


nodesView : List Node -> Element Msg
nodesView nodes =
    Element.column []
        (List.map nodeView nodes)


nodeView : Node -> Element Msg
nodeView node =
    Element.text (node.id ++ " " ++ node.name)


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
