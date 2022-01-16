port module Login exposing (Msg, update, view)

import Element exposing (..)
import Element.Input as Input


port signIn : () -> Cmd msg



-- UPDATE


type Msg
    = SignIn


update : Msg -> Cmd msg
update msg =
    case msg of
        SignIn ->
            signIn ()



-- VIEW


view : Element Msg
view =
    Element.column [ width fill, height fill ]
        [ Element.el [ width fill, height (px 200) ] signInButton ]


signInButton =
    Input.button
        [ centerX, centerY ]
        { label = Element.text "Sign In"
        , onPress = Just SignIn
        }
