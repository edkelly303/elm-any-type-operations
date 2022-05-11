module Order exposing
    ( Interface
    , makeInterface
    , Op
    , eq
    , neq
    , gt
    , gte
    , lt
    , lte
    )

{-| The `Order` module allows you to generate a bunch of useful functions (an
`Interface`) that you can use to compare the custom types or record types that you
define in your Elm applications.

These functions will work even though custom types and record types are not
included in Elm's built-in `comparable` typeclass.


## What is an Interface?

An `Interface` is just a record containing functions that can be used to
compare your custom types.

The type signature might look daunting, but in practice you can ignore it.
All you need to remember is that if you create an `Interface` called `order`,
you can call the functions within it by doing `order.compare`, `order.greater`,
and so on:

    type Example
        = A
        | B
        | C

    toInt : Example -> Int
    toInt example =
        case example of
            A ->
                0

            B ->
                1

            C ->
                2

    order =
        Order.makeInterface toInt

    thisIsTrue =
        order.greater A B == B


## Interface type

@docs Interface


## Creating an interface

@docs makeInterface


## Operators

@docs Op

@docs eq

@docs neq

@docs gt

@docs gte

@docs lt

@docs lte

-}


{-| The `Interface` type is a record containing functions that you can use to
compare the values of your custom types, even though Elm's custom types are not
`comparable`.

The available functions are:


## `op`

Provides an equivalent to Elm's comparison operators `(==)`, `(/=)`, `(>)`,
`(<)`, `(>=)` and `(<=)`, so you can compare two values of your custom type and
get a `Bool` in response.

    order.op A eq A == True

    order.op A neq A == False

    order.op A gt B == False

    order.op A lt B == True

    order.op B gte C == False

    order.op A lte A == True


## `compare`

The equivalent of `Basics.compare`

    order.compare A B == Basics.LT

    order.compare B A == Basics.GT

    order.compare A A == Basics.EQ


## `greater`

The equivalent of `Basics.max`

    order.greater A B == B

    order.greater C A == C


## `lesser`

The equivalent of `Basics.min`

    order.lesser A B == A

    order.lesser C A == A

(I always get confused with `min` and `max`; I think `greater` and `lesser` are
clearer names.)


## `greatest`

The equivalent of `List.maximum`

    order.greatest [] == Nothing

    order.greatest [ A, B, C ] == Just C


## `least`

The equivalent of `List.minimum`

    order.least [] == Nothing

    order.least [ A, B, C ] == Just A

(Similarly, I think `greatest` and `least` are clearer than `maximum` and
`minimum`.)

-}
type alias Interface a =
    { op : a -> Op -> a -> Bool
    , compare : a -> a -> Basics.Order
    , greater : a -> a -> a
    , lesser : a -> a -> a
    , greatest : List a -> Maybe a
    , least : List a -> Maybe a
    }


{-| To define an `Interface`, you just need to supply a function that converts
your custom type to a `comparable` (i.e. a `String`, `Int` or `Float`, or a
`Tuple` that contains only comparable values).

Here's an example for "enum" types

    type Animal
        = Aardvark
        | Bear
        | Capybara

    animalToString : Animal -> String
    animalToString example =
        case example of
            Aardvark ->
                "aardvark"

            Bear ->
                "bear"

            Capybara ->
                "capybara"

    order : Interface Animal
    order =
        Order.makeInterface animalToString

    thisIsTrue =
        order.op Aardvark lte Bear == True

And here's an example for "wrapper" types

    type Wrapper
        = Wrapper Int

    wrapperToInt : Wrapper -> Int
    wrapperToInt (Wrapper int) =
        int

    order : Order Wrapper
    order =
        Order.makeInterface wrapperToInt

    thisIsTrueToo =
        order.greatest
            [ Wrapper 1
            , Wrapper 2
            , Wrapper 3
            ]
            == Just (Wrapper 3)

-}
makeInterface : (a -> comparable) -> Interface a
makeInterface toComparable =
    { op = op toComparable
    , compare = compare toComparable
    , greater = greater toComparable
    , lesser = lesser toComparable
    , greatest = greatest toComparable
    , least = least toComparable
    }


{-| A type comprising the comparison operators that you can use with the `op`
function
-}
type Op
    = Eq
    | Neq
    | Gt
    | Lt
    | Gte
    | Lte


{-| The equal-to operator, equivalent to `(==)`
-}
eq : Op
eq =
    Eq


{-| The not-equal-to operator, equivalent to `(/=)`
-}
neq : Op
neq =
    Neq


{-| The greater-than operator, equivalent to `(>)`
-}
gt : Op
gt =
    Gt


{-| The less-than operator, equivalent to `(<)`
-}
lt : Op
lt =
    Lt


{-| The greater-than-or-equal-to operator, equivalent to `(>=)`
-}
gte : Op
gte =
    Gte


{-| The less-than-or-equal-to operator, equivalent to `(<=)`
-}
lte : Op
lte =
    Lte



-- INTERNALS


op : (a -> comparable) -> a -> Op -> a -> Bool
op toComparable arg1 operator arg2 =
    opToFunction operator (toComparable arg1) (toComparable arg2)


opToFunction : Op -> (comparable -> comparable -> Bool)
opToFunction operator =
    case operator of
        Eq ->
            (==)

        Neq ->
            (/=)

        Gt ->
            (>)

        Lt ->
            (<)

        Gte ->
            (>=)

        Lte ->
            (<=)


compare : (a -> comparable) -> a -> a -> Basics.Order
compare toComparable arg1 arg2 =
    Basics.compare (toComparable arg1) (toComparable arg2)


greater : (a -> comparable) -> a -> a -> a
greater toComparable arg1 arg2 =
    case compare toComparable arg1 arg2 of
        Basics.GT ->
            arg1

        Basics.EQ ->
            arg1

        Basics.LT ->
            arg2


lesser : (a -> comparable) -> a -> a -> a
lesser toComparable arg1 arg2 =
    case compare toComparable arg1 arg2 of
        Basics.GT ->
            arg2

        Basics.EQ ->
            arg1

        Basics.LT ->
            arg1


greatest : (a -> comparable) -> List a -> Maybe a
greatest toComparable list =
    List.foldl
        (\item result ->
            case result of
                Nothing ->
                    Just item

                Just prevItem ->
                    Just (greater toComparable item prevItem)
        )
        Nothing
        list


least : (a -> comparable) -> List a -> Maybe a
least toComparable list =
    List.foldl
        (\item result ->
            case result of
                Nothing ->
                    Just item

                Just prevItem ->
                    Just (lesser toComparable item prevItem)
        )
        Nothing
        list
