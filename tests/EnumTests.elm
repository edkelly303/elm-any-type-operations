module EnumTests exposing (suite)

import Enum
import Expect
import Test exposing (..)


type A
    = A
    | B
    | C


enum : Enum.Interface A
enum =
    Enum.makeInterface A [ B, C ]


suite : Test
suite =
    describe "Enum"
        [ describe ".all"
            [ test "returns list of all items" <|
                \_ ->
                    enum.all
                        |> Expect.equal [ A, B, C ]
            ]
        , describe ".first"
            [ test "returns first item" <|
                \_ ->
                    enum.first
                        |> Expect.equal A
            ]
        , describe ".last"
            [ test "returns last item" <|
                \_ ->
                    enum.last
                        |> Expect.equal C
            ]
        , describe ".next"
            [ test "returns next item if there is one" <|
                \_ ->
                    enum.next B
                        |> Expect.equal (Just C)
            , test "returns Nothing if there is no next item" <|
                \_ ->
                    enum.next C
                        |> Expect.equal Nothing
            ]
        , describe ".previous"
            [ test "returns previous item if there is one" <|
                \_ ->
                    enum.previous B
                        |> Expect.equal (Just A)
            , test "returns Nothing if there is no previous item" <|
                \_ ->
                    enum.previous A
                        |> Expect.equal Nothing
            ]
        ]
