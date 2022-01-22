module ExpandableList exposing (expandableList)

import Element exposing (..)
import Element.Input as Input


expandableList : msg -> Bool -> List (Element msg) -> Element msg
expandableList toggleMsg expanded allItems =
    let
        ( visibleItems, expander ) =
            case ( expanded, List.length allItems < 2 ) of
                ( _, True ) ->
                    ( allItems, Element.none )

                ( False, _ ) ->
                    ( List.take 1 allItems, button toggleMsg expanded )

                ( True, _ ) ->
                    ( allItems, button toggleMsg expanded )
    in
    Element.row [ spacing 10, width fill ]
        [ Element.column [ width fill ] visibleItems
        , expander
        ]


button : msg -> Bool -> Element msg
button toggleMsg expanded =
    let
        label =
            case expanded of
                False ->
                    "[+]"

                True ->
                    "[-]"
    in
    Input.button
        [ alignTop ]
        { label = Element.text label
        , onPress = Just toggleMsg
        }
