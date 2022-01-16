port module Header exposing (Msg, update, view)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input



-- PORTS


port signOut : () -> Cmd msg



-- UPDATE


type Msg
    = SignOut


update : Msg -> Cmd msg
update msg =
    case msg of
        SignOut ->
            signOut ()



-- VIEW


view : Element Msg
view =
    Element.row
        [ padding 10
        , width fill
        , Font.color lightBlue
        , Background.color (rgb255 66 135 245)
        ]
        [ Element.el [ alignLeft ] (Element.text "Ficus")
        , Element.el [ alignRight ] signOutButton
        ]


signOutButton =
    Input.button
        []
        { label = Element.text "Sign Out"
        , onPress = Just SignOut
        }
