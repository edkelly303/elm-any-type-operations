module Enum exposing
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

{-| The `Enum` module allows you to generate a bunch of useful functions (an
`Interface`) that you can use to work with custom types that have a finite number
of variants, aka enums.

For example, this is an enum:

    type Season
        = Spring
        | Summer
        | Autumn
        | Winter

Because it only has a finite number of possible values (4).

This _isn't_ an enum:

    type Id
        = Id Int

Because there is an infinite series of integers, so you can't fully enumerate
all the values of this type.

The `Enum` module gives you all the same functions as the `Order` module, plus a few
extra ones that may come in handy when working with enums.


## What is an Interface?

An `Interface` is just a record containing functions that you can apply to your
enum types.

The type signature might look daunting, but in practice you can ignore it.
All you need to remember is that if you create an `Interface` called `enum`,
you can call the functions within it by doing `enum.all`, `enum.first`,
`enum.next`, and so on:

    type Example
        = A
        | B
        | C

    enum =
        Enum.makeInterface A B [ C ]

    thisIsTrue =
        enum.next A == Just B


## Interface type

@docs Interface


## Creating an Interface

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

import Order


{-| The `Interface` type is a record containing functions that you can use to
compare the values of your custom types, even though Elm's custom types are not
`comparable`.

The available functions are:


## `op`

Provides an equivalent to Elm's comparison operators `(==)`, `(/=)`, `(>)`,
`(<)`, `(>=)` and `(<=)`, so you can compare two values of your custom type and
get a `Bool` in response.

    enum.op A eq A == True

    enum.op A neq A == False

    enum.op A gt B == False

    enum.op A lt B == True

    enum.op B gte C == False

    enum.op A lte A == True


## `compare`

The equivalent of `Basics.compare`

    enum.compare A B == Basics.LT

    enum.compare B A == Basics.GT

    enum.compare A A == Basics.EQ


## `greater`

The equivalent of `Basics.max`

    enum.greater A B == B

    enum.greater C A == C


## `lesser`

The equivalent of `Basics.min`

    enum.lesser A B == A

    enum.lesser C A == A

(I always get confused with `min` and `max`; I think `greater` and `lesser` are
clearer names.)


## `greatest`

The equivalent of `List.maximum`

    enum.greatest [] == Nothing

    enum.greatest [ A, B, C ] == Just C


## `least`

The equivalent of `List.minimum`

    enum.least [] == Nothing

    enum.least [ A, B, C ] == Just A

(Similarly, I think `greatest` and `least` are clearer than `maximum` and
`minimum`.)


## `all`

Returns a list of all the values of the enum, in order from least to greatest

    enum.all == [ A, B, C ]


## `first`

Returns the first (least) value of the enum

    enum.first == A


## `last`

Returns the last (greatest) value of the enum

    enum.last == C


## `next`

Given a value of the enum, returns the next (greater) value, or Nothing if no greater value exists

    enum.next B == Just C

    enum.next C == Nothing


## `previous`

Given a value of the enum, returns the previous (lesser) value, or Nothing if no lesser value exists

    enum.previous B == Just A

    enum.previous A == Nothing

-}
type alias Interface enum =
    { op : enum -> Order.Op -> enum -> Bool
    , compare : enum -> enum -> Basics.Order
    , greater : enum -> enum -> enum
    , lesser : enum -> enum -> enum
    , greatest : List enum -> Maybe enum
    , least : List enum -> Maybe enum
    , all : List enum
    , first : enum
    , last : enum
    , next : enum -> Maybe enum
    , previous : enum -> Maybe enum
    }


{-| To define an `Interface`, you just need to supply all the values of the
enum, in order from least to greatest.

It only makes sense to treat a custom type as an enum if it has two or more
variants, so the API for `makeInterface` requires you to supply at least two
values as the first two arguments. If there are more than two values, you can
put the rest in a list as the third argument.

    type Season
        = Spring
        | Summer
        | Autumn
        | Winter

    enum =
        Enum.makeInterface Spring Summer [ Autumn, Winter ]

    thisIsTrue =
        enum.next Summer == Autumn

    thisIsTrueToo =
        enum.op Winter gt Autumn == True

-}
makeInterface : enum -> enum -> List enum -> Interface enum
makeInterface first second rest =
    let
        idxList =
            List.indexedMap (\idx enum -> ( enum, idx )) (first :: second :: rest)

        toComparable enum =
            List.foldl
                (\( e, idx ) prev ->
                    if e == enum then
                        Just idx

                    else
                        prev
                )
                Nothing
                idxList
                |> Maybe.withDefault 0

        order =
            Order.makeInterface toComparable
    in
    { op = order.op
    , compare = order.compare
    , greater = order.greater
    , lesser = order.lesser
    , greatest = order.greatest
    , least = order.least
    , all = first :: second :: rest
    , first = first
    , last = last first (second :: rest)
    , next = next first (second :: rest)
    , previous = previous first (second :: rest)
    }


last : enum -> List enum -> enum
last first rest =
    List.foldl (\enum _ -> enum) first rest


previous : enum -> List enum -> enum -> Maybe enum
previous first rest this =
    List.foldl
        (\enum ( prev, found ) ->
            if found then
                ( prev, found )

            else if enum == this then
                ( prev, True )

            else
                ( enum, False )
        )
        ( first, False )
        rest
        |> (\( enum, found ) ->
                if found then
                    Just enum

                else
                    Nothing
           )


next : enum -> List enum -> enum -> Maybe enum
next first rest this =
    List.foldl
        (\enum ( prev, _ ) ->
            if prev == this then
                ( enum, True )

            else
                ( enum, False )
        )
        ( first, False )
        rest
        |> (\( enum, found ) ->
                if found then
                    Just enum

                else
                    Nothing
           )



-- RE-EXPORTS FROM ORDER


{-| A type comprising the comparison operators that you can use with the `op`
function. (This is re-exported from the `Order` module for convenience.)
-}
type alias Op =
    Order.Op


{-| The equal-to operator, equivalent to `(==)`. (This is re-exported from the
`Order` module for convenience.)
-}
eq : Order.Op
eq =
    Order.eq


{-| The not-equal-to operator, equivalent to `(/=)`. (This is re-exported from the
`Order` module for convenience.)
-}
neq : Order.Op
neq =
    Order.neq


{-| The greater-than operator, equivalent to `(>)`. (This is re-exported from
the `Order` module for convenience.)
-}
gt : Order.Op
gt =
    Order.gt


{-| The less-than operator, equivalent to `(<)`. (This is re-exported from the
`Order` module for convenience.)
-}
lt : Order.Op
lt =
    Order.lt


{-| The greater-than-or-equal-to operator, equivalent to `(>=)`. (This is
re-exported from the `Order` module for convenience.)
-}
gte : Order.Op
gte =
    Order.gte


{-| The less-than-or-equal-to operator, equivalent to `(<=)`. (This is
re-exported from the `Order` module for convenience.)
-}
lte : Order.Op
lte =
    Order.lte
