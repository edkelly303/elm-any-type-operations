module ArithmeticTests exposing (suite)

import Arithmetic exposing (add, div, mul, sub)
import Expect
import Test exposing (..)


type A
    = A Int


arith : Arithmetic.Interface A
arith =
    Arithmetic.makeIntegerInterface
        { toInt = \(A int) -> int
        , fromInt = A
        }


suite : Test
suite =
    describe ".op"
        [ test "Add" <|
            \_ ->
                arith.op (A 6) add (A 2)
                    |> Expect.equal (A 8)
        , test "Sub" <|
            \_ ->
                arith.op (A 6) sub (A 2)
                    |> Expect.equal (A 4)
        , test "Mul" <|
            \_ ->
                arith.op (A 6) mul (A 2)
                    |> Expect.equal (A 12)
        , test "Div" <|
            \_ ->
                arith.op (A 6) div (A 2)
                    |> Expect.equal (A 3)
        ]
