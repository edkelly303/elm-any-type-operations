module OrderTests exposing (suite)

import Expect
import Order exposing (Op(..))
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


order : Order.Interface A
order =
    Order.makeInterface toInt


suite : Test
suite =
    describe "Order"
        [ describe ".op"
            [ test "Eq returns True when arguments are equal" <|
                \_ ->
                    order.op A Eq A
                        |> Expect.equal True
            , test "Eq returns False when arguments are not equal" <|
                \_ ->
                    order.op A Eq B
                        |> Expect.equal False
            , test "Neq returns False when arguments are equal" <|
                \_ ->
                    order.op A Neq A
                        |> Expect.equal False
            , test "Neq returns True when arguments are not equal" <|
                \_ ->
                    order.op A Neq B
                        |> Expect.equal True
            , test "Gt returns True when arg1 is greater than arg2" <|
                \_ ->
                    order.op B Gt A
                        |> Expect.equal True
            , test "Gt returns False when arg1 is less than arg2" <|
                \_ ->
                    order.op A Gt B
                        |> Expect.equal False
            , test "Gt returns False when args are equal" <|
                \_ ->
                    order.op A Gt A
                        |> Expect.equal False
            , test "Lt returns False when arg1 is greater than arg2" <|
                \_ ->
                    order.op B Lt A
                        |> Expect.equal False
            , test "Lt returns False when args are equal" <|
                \_ ->
                    order.op A Lt A
                        |> Expect.equal False
            , test "Lt returns True when arg1 is less than arg2" <|
                \_ ->
                    order.op A Lt B
                        |> Expect.equal True
            , test "Gte returns True when arg1 is greater than arg2" <|
                \_ ->
                    order.op B Gte A
                        |> Expect.equal True
            , test "Gte returns True when args are equal" <|
                \_ ->
                    order.op A Gte A
                        |> Expect.equal True
            , test "Gte returns False when arg1 is less than arg2" <|
                \_ ->
                    order.op A Gte B
                        |> Expect.equal False
            , test "Lte returns False when arg1 is greater than arg2" <|
                \_ ->
                    order.op B Lte A
                        |> Expect.equal False
            , test "Lte returns True when arg1 is less than arg2" <|
                \_ ->
                    order.op A Lte B
                        |> Expect.equal True
            , test "Lte returns True when args are equal" <|
                \_ ->
                    order.op A Lte A
                        |> Expect.equal True
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
        , describe ".greatest"
            [ test "returns Nothing for empty list" <|
                \_ ->
                    order.greatest []
                        |> Expect.equal Nothing
            , test "returns first item when it is the greatest" <|
                \_ ->
                    order.greatest [ B, A ]
                        |> Expect.equal (Just B)
            , test "returns second item when it is the greatest" <|
                \_ ->
                    order.greatest [ A, C, B ]
                        |> Expect.equal (Just C)
            , test "returns first item when all items are equal" <|
                \_ ->
                    order.greatest [ A, A, A ]
                        |> Expect.equal (Just A)
            ]
        , describe ".least"
            [ test "returns Nothing for empty list" <|
                \_ ->
                    order.least []
                        |> Expect.equal Nothing
            , test "returns first item when it is the least" <|
                \_ ->
                    order.least [ A, B ]
                        |> Expect.equal (Just A)
            , test "returns second item when it is the least" <|
                \_ ->
                    order.least [ C, A, B ]
                        |> Expect.equal (Just A)
            , test "returns first item when all items are equal" <|
                \_ ->
                    order.least [ A, A, A ]
                        |> Expect.equal (Just A)
            ]
        ]
