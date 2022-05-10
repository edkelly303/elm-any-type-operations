module Enum exposing (..)

import Order


type alias Interface enum comparable =
    { op : enum -> (comparable -> comparable -> Bool) -> enum -> Bool
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
    , cycleNext : enum -> enum
    , cyclePrevious : enum -> enum
    }


makeInterface : enum -> List enum -> Interface enum Int
makeInterface first rest =
    let
        idxList =
            List.indexedMap (\idx enum -> ( enum, idx )) (first :: rest)

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
    , all = first :: rest
    , first = first
    , last = last first rest
    , next = next first rest
    , previous = previous first rest
    , cycleNext = cycleNext first rest
    , cyclePrevious = cyclePrevious first rest
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


cyclePrevious : enum -> List enum -> enum -> enum
cyclePrevious first rest this =
    previous first rest this
        |> Maybe.withDefault (last first rest)


cycleNext : enum -> List enum -> enum -> enum
cycleNext first rest this =
    next first rest this
        |> Maybe.withDefault first
