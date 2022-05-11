module Arithmetic exposing
    ( Interface
    , makeIntegerInterface
    , makeFloatInterface
    , makeInterface
    , Op
    , add
    , sub
    , mul
    , div
    , div0
    , divM
    )

{-| The `Arithmetic` module allows you to generate a function (`Interface`) that you
can use to perform arithmetic operations easily on your custom and record types.


## What is an Interface?

An `Interface` is just a function that you can define at the top level of your
modules to specify how to apply arithmetic operations to your types.

If you create an `Interface` called `arith`, you can then perform calculations
as follows:

    import Arithmetic exposing (add, makeIntegerInterface)

    type Length
        = Length Int

    arith =
        makeIntegerInterface
            { toInt = \(Length int) -> int
            , fromInt = Length
            }

    thisIsTrue =
        arith (Length 1) add (Length 2) == Length 3


## Interface type

@docs Interface


## Creating an Interface

@docs makeIntegerInterface

@docs makeFloatInterface

@docs makeInterface


## Operators

@docs Op

@docs add

@docs sub

@docs mul

@docs div

@docs div0

@docs divM

-}


{-| An interface is just a function that you can use to perform arithmetic with
whatever types you've defined. Use one of the `makeInterface` functions to
define an interface at the top level of the same module where you've defined
your type.
-}
type alias Interface a =
    a -> Op -> a -> a


{-| A type comprising the arithmetic operators that you can use with the
Interface function
-}
type Op
    = Add
    | Sub
    | Mul
    | Div
    | Div0
    | DivM


{-| Equivalent to `(+)`, or whatever the equivalent addition operation should be
for your type.

    arith (Length 6) add (Length 2) == Length 8

-}
add : Op
add =
    Add


{-| Equivalent to `(-)`, or whatever the equivalent subtraction operation should
be for your type.

    arith (Length 6) sub (Length 2) == Length 4

-}
sub : Op
sub =
    Sub


{-| Equivalent to `(*)`, or whatever the equivalent multiplication operation
should be for your type.

    arith (Length 6) mul (Length 2) == Length 12

-}
mul : Op
mul =
    Mul


{-| Equivalent to `(//)` or `(/)`, or whatever the equivalent division operation
should be for your type.

    arith (Length 6) div (Length 2) == Length 3

-}
div : Op
div =
    Div


{-| Equivalent to `(//)` or `(/)`, or whatever the equivalent division operation
should be for your type, except that division by zero returns zero.

    arith (Length 6) div0 (Length 2) == Length 3

    arith (Length 6) div0 (Length 0) == Length 0

-}
div0 : Op
div0 =
    Div0


{-| Equivalent to `(//)` or `(/)`, or whatever the equivalent division operation
should be for your type, except that division by zero returns the lowest
negative number.

    arith (Length 6) divM (Length 2) == Length 3

    arith (Length 6) divM (Length 0) == Length -2147483648

-}
divM : Op
divM =
    DivM


{-| Create an interface that can perform integer arithmetic on your custom type.

    arith =
        makeIntegerInterface
            { toInt = \(Length int) -> int
            , fromInt = Length
            }

This gives you the following possibilities:

    arith (Length 6) add (Length 2) == Length 8

    arith (Length 6) sub (Length 2) == Length 4

    arith (Length 6) mul (Length 2) == Length 12

    arith (Length 6) div (Length 2) == Length 3

There are also two other variants of `div`, called `div0` and `divM`.
These behave the same as `div`, except in their handling of division by zero.

    -- `div`: division by zero causes a runtime exception
    arith (Length 6) div (Length 0) == Debug.todo "CRASH!"

    -- `div0`: division by zero returns zero
    arith (Length 6) div0 (Length 0) == Length 0

    -- `divM`: division by zero returns Elm's minimum integer
    arith (Length 6) divM (Length 0) == Length -2147483648

-}
makeIntegerInterface : { toInt : a -> Int, fromInt : Int -> a } -> Interface a
makeIntegerInterface { toInt, fromInt } =
    let
        calc operator arg1 arg2 =
            operator (toInt arg1) (toInt arg2)
                |> fromInt
    in
    makeInterface
        { add = calc (+)
        , sub = calc (-)
        , mul = calc (*)
        , div = calc (//)
        , div0 =
            \arg1 arg2 ->
                if toInt arg2 == 0 then
                    fromInt 0

                else
                    calc (//) arg1 arg2
        , divM =
            \arg1 arg2 ->
                if toInt arg2 == 0 then
                    fromInt lowestInt

                else
                    calc (//) arg1 arg2
        }


{-| Create an interface that can perform float arithmetic on your custom type.

    type Width
        = Width Float

    arith =
        makeFloatInterface
            { toFloat = \(Width flt) -> flt
            , fromFloat = Width
            }

This gives you the following possibilities:

    arith (Width 6.0) add (Width 2.0) == Width 8.0

    arith (Width 6.0) sub (Width 2.0) == Width 4.0

    arith (Width 6.0) mul (Width 2.0) == Width 12.0

    arith (Width 6.0) div (Width 2.0) == Width 3.0

There are also two other variants of `div`, called `div0` and `divM`.
These behave the same as `div`, except in their handling of division by zero.

    -- `div`: dividing a non-zero number by zero returns Infinity
    arith (Width 1.0) div (Width 0.0) == Width Infinity

    -- `div`: dividing zero by zero returns NaN
    arith (Width 0.0) div (Width 0.0) == Width NaN

    -- `div0`: division by zero returns zero
    arith (Width 6.0) div0 (Width 0.0) == Width 0.0

    -- `divM`: division by zero returns Elm's minimum float
    arith (Width 6.0) divM (Width 0.0) == Width -1.79e308

-}
makeFloatInterface : { toFloat : a -> Float, fromFloat : Float -> a } -> Interface a
makeFloatInterface { toFloat, fromFloat } =
    let
        calc operator arg1 arg2 =
            operator (toFloat arg1) (toFloat arg2)
                |> fromFloat
    in
    makeInterface
        { add = calc (+)
        , sub = calc (-)
        , mul = calc (*)
        , div = calc (/)
        , div0 =
            \arg1 arg2 ->
                if toFloat arg2 == 0.0 then
                    fromFloat 0.0

                else
                    calc (/) arg1 arg2
        , divM =
            \arg1 arg2 ->
                if toFloat arg2 == 0.0 then
                    fromFloat lowestFloat

                else
                    calc (/) arg1 arg2
        }


{-| A more generic way to create an Interface, which you might need to use if
you want to define your own custom arithmetic rules.

As a silly example, here's a way to reimplement Elm's integer arithmetic and
supplement it with `div0` and `divM`:

    arith =
        makeInterface
            { add = (+)
            , sub = (-)
            , mul = (*)
            , div = (//)
            , div0 =
                \a1 a2 ->
                    if a2 == 0 then
                        0

                    else
                        a1 // a2
            , divM =
                \a1 a2 ->
                    if a2 == 0 then
                        -2147483648

                    else
                        a1 // a2
            }

    thisIsTrue =
        arith 10 div0 2 == 5

-}
makeInterface :
    { add : a -> a -> a
    , sub : a -> a -> a
    , mul : a -> a -> a
    , div : a -> a -> a
    , div0 : a -> a -> a
    , divM : a -> a -> a
    }
    -> Interface a
makeInterface config =
    op config


op :
    { add : a -> a -> a
    , sub : a -> a -> a
    , mul : a -> a -> a
    , div : a -> a -> a
    , div0 : a -> a -> a
    , divM : a -> a -> a
    }
    -> a
    -> Op
    -> a
    -> a
op c arg1 operator arg2 =
    let
        opFn =
            case operator of
                Add ->
                    c.add

                Sub ->
                    c.sub

                Mul ->
                    c.mul

                Div ->
                    c.div

                Div0 ->
                    c.div0

                DivM ->
                    c.divM
    in
    opFn arg1 arg2


lowestInt : Int
lowestInt =
    -2147483648


lowestFloat : Float
lowestFloat =
    -1.79e308
