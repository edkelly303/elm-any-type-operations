module ArithmeticTests exposing (suite)

import Arithmetic exposing (add, div, div0, divM, mul, sub)
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
                arith (A 6) add (A 2)
                    |> Expect.equal (A 8)
        , test "Sub" <|
            \_ ->
                arith (A 6) sub (A 2)
                    |> Expect.equal (A 4)
        , test "Mul" <|
            \_ ->
                arith (A 6) mul (A 2)
                    |> Expect.equal (A 12)
        , test "Div" <|
            \_ ->
                arith (A 6) div (A 2)
                    |> Expect.equal (A 3)
        , test "Div0" <|
            \_ ->
                arith (A 6) div0 (A 2)
                    |> Expect.equal (A 3)
        , test "Div0 of 0" <|
            \_ ->
                arith (A 1) div0 (A 0)
                    |> Expect.equal (A 0)
        , test "DivM" <|
            \_ ->
                arith (A 6) divM (A 2)
                    |> Expect.equal (A 3)
        , test "DivM of 0" <|
            \_ ->
                arith (A 1) divM (A 0)
                    |> Expect.equal (A -2147483648)
        ]
