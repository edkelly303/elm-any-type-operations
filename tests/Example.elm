module Example exposing (suite)

import Expect
import Order
import Test exposing (..)


type A
    = A
    | B
    | C


toInt : A -> Int
toInt x =
    case x of
        A ->
            1

        B ->
            2

        C ->
            3


suite : Test
suite =
    let
        order =
            Order.makeInterface toInt
    in
    describe "Order"
        [ describe ".op"
            [ test "(==) returns True when arguments are equal" <|
                \_ ->
                    order.op A (==) A
                        |> Expect.equal True
            , test "(==) returns False when arguments are not equal" <|
                \_ ->
                    order.op A (==) B
                        |> Expect.equal False
            ]
        , describe ".compare"
            [ test "returns EQ when arguments are equal" <|
                \_ ->
                    order.compare A A
                        |> Expect.equal Basics.EQ
            , test "returns GT when first arg is greater than second arg" <|
                \_ ->
                    order.compare B A
                        |> Expect.equal Basics.GT
            , test "returns LT when first arg is greater than second arg" <|
                \_ ->
                    order.compare A B
                        |> Expect.equal Basics.LT
            ]
        , describe ".greater"
            [ test "returns second arg when it is greater than first arg" <|
                \_ ->
                    order.greater A B
                        |> Expect.equal B
            , test "returns first arg when it is greater than second arg" <|
                \_ ->
                    order.greater B A
                        |> Expect.equal B
            , test "returns first arg when args are equal" <|
                \_ ->
                    order.greater A A
                        |> Expect.equal A
            ]
        , describe ".lesser"
            [ test "returns second arg when it is lesser than first arg" <|
                \_ ->
                    order.lesser B A
                        |> Expect.equal A
            , test "returns first arg when it is lesser than second arg" <|
                \_ ->
                    order.lesser A B
                        |> Expect.equal A
            , test "returns first arg when args are equal" <|
                \_ ->
                    order.lesser A A
                        |> Expect.equal A
            ]
        ]