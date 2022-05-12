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
        }


{-| A more generic way to create an Interface, which you might need to use if
you want to define your own custom arithmetic rules.

As an example, here's a way to reimplement integer arithmetic, but with 
an alternative division operator that returns Elm's minimum integer 
(-2147483648) when you divide by zero, instead of raising a runtime exception:

    arith =
        makeInterface
            { add = (+)
            , sub = (-)
            , mul = (*)
            , div =
                \a1 a2 ->
                    if a2 == 0 then
                        -2147483648

                    else
                        a1 // a2
            }

    thisIsTrue =
        arith 1 div 0 == -2147483648

-}
makeInterface :
    { add : a -> a -> a
    , sub : a -> a -> a
    , mul : a -> a -> a
    , div : a -> a -> a
    }
    -> Interface a
makeInterface config =
    op config


op :
    { add : a -> a -> a
    , sub : a -> a -> a
    , mul : a -> a -> a
    , div : a -> a -> a
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
    in
    opFn arg1 arg2
