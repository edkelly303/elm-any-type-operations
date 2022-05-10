module EnumTests exposing (suite)

import Enum
import Expect
import Test exposing (..)


type A
    = A
    | B
    | C


enum : Enum.Interface A Int
enum =
    Enum.makeInterface A [ B, C ]


suite : Test
suite =
    describe "Enum"
        [ describe ".op"
            [ test "(==) returns True when arguments are equal" <|
                \_ ->
                    enum.op A (==) A
                        |> Expect.equal True
            , test "(==) returns False when arguments are not equal" <|
                \_ ->
                    enum.op A (==) B
                        |> Expect.equal False
            , test "(/=) returns False when arguments are equal" <|
                \_ ->
                    enum.op A (/=) A
                        |> Expect.equal False
            , test "(/=) returns True when arguments are not equal" <|
                \_ ->
                    enum.op A (/=) B
                        |> Expect.equal True
            , test "(>) returns True when arg1 is greater than arg2" <|
                \_ ->
                    enum.op B (>) A
                        |> Expect.equal True
            , test "(>) returns False when arg1 is less than arg2" <|
                \_ ->
                    enum.op A (>) B
                        |> Expect.equal False
            , test "(<) returns False when arg1 is greater than arg2" <|
                \_ ->
                    enum.op B (<) A
                        |> Expect.equal False
            , test "(<) returns True when arg1 is less than arg2" <|
                \_ ->
                    enum.op A (<) B
                        |> Expect.equal True
            , test "(>=) returns True when arg1 is greater than arg2" <|
                \_ ->
                    enum.op B (>=) A
                        |> Expect.equal True
            , test "(>=) returns True when args are equal" <|
                \_ ->
                    enum.op A (>=) A
                        |> Expect.equal True
            , test "(>=) returns False when arg1 is less than arg2" <|
                \_ ->
                    enum.op A (>=) B
                        |> Expect.equal False
            , test "(<=) returns False when arg1 is greater than arg2" <|
                \_ ->
                    enum.op B (<=) A
                        |> Expect.equal False
            , test "(<=) returns True when arg1 is less than arg2" <|
                \_ ->
                    enum.op A (<=) B
                        |> Expect.equal True
            , test "(<=) returns True when args are equal" <|
                \_ ->
                    enum.op A (<=) A
                        |> Expect.equal True
            ]
        , describe ".compare"
            [ test "returns EQ when arguments are equal" <|
                \_ ->
                    enum.compare A A
                        |> Expect.equal Basics.EQ
            , test "returns GT when first arg is greater than second arg" <|
                \_ ->
                    enum.compare B A
                        |> Expect.equal Basics.GT
            , test "returns LT when first arg is greater than second arg" <|
                \_ ->
                    enum.compare A B
                        |> Expect.equal Basics.LT
            ]
        , describe ".greater"
            [ test "returns second arg when it is greater than first arg" <|
                \_ ->
                    enum.greater A B
                        |> Expect.equal B
            , test "returns first arg when it is greater than second arg" <|
                \_ ->
                    enum.greater B A
                        |> Expect.equal B
            , test "returns first arg when args are equal" <|
                \_ ->
                    enum.greater A A
                        |> Expect.equal A
            ]
        , describe ".lesser"
            [ test "returns second arg when it is lesser than first arg" <|
                \_ ->
                    enum.lesser B A
                        |> Expect.equal A
            , test "returns first arg when it is lesser than second arg" <|
                \_ ->
                    enum.lesser A B
                        |> Expect.equal A
            , test "returns first arg when args are equal" <|
                \_ ->
                    enum.lesser A A
                        |> Expect.equal A
            ]
        , describe ".greatest"
            [ test "returns Nothing for empty list" <|
                \_ ->
                    enum.greatest []
                        |> Expect.equal Nothing
            , test "returns first item when it is the greatest" <|
                \_ ->
                    enum.greatest [ B, A ]
                        |> Expect.equal (Just B)
            , test "returns second item when it is the greatest" <|
                \_ ->
                    enum.greatest [ A, C, B ]
                        |> Expect.equal (Just C)
            , test "returns first item when all items are equal" <|
                \_ ->
                    enum.greatest [ A, A, A ]
                        |> Expect.equal (Just A)
            ]
        , describe ".least"
            [ test "returns Nothing for empty list" <|
                \_ ->
                    enum.least []
                        |> Expect.equal Nothing
            , test "returns first item when it is the least" <|
                \_ ->
                    enum.least [ A, B ]
                        |> Expect.equal (Just A)
            , test "returns second item when it is the least" <|
                \_ ->
                    enum.least [ C, A, B ]
                        |> Expect.equal (Just A)
            , test "returns first item when all items are equal" <|
                \_ ->
                    enum.least [ A, A, A ]
                        |> Expect.equal (Just A)
            ]
        , describe ".all"
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
        , describe ".cycleNext"
            [ test "returns next item if there is one" <|
                \_ ->
                    enum.cycleNext B
                        |> Expect.equal C
            , test "returns first item if there is no next item" <|
                \_ ->
                    enum.cycleNext C
                        |> Expect.equal A
            ]
        , describe ".cyclePrevious"
            [ test "returns previous item if there is one" <|
                \_ ->
                    enum.cyclePrevious B
                        |> Expect.equal A
            , test "returns last item if there is no previous item" <|
                \_ ->
                    enum.cyclePrevious A
                        |> Expect.equal C
            ]
        ]
