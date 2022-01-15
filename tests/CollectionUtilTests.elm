module CollectionUtilTests exposing (..)

import CollectionUtil exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "CollectionUtil module"
        [ describe "listWithDefault"
            [ test "empty list is replaced by the default item" <|
                \_ -> Expect.equal [ "a" ] (listWithDefault "a" [])
            , test "list with single element is not affected" <|
                \_ -> Expect.equal [ "b" ] (listWithDefault "a" [ "b" ])
            , test "list with two elements is not affected" <|
                \_ -> Expect.equal [ "b", "c" ] (listWithDefault "a" [ "b", "c" ])
            ]
        ]
