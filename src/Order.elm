module Order exposing (makeInterface)

{-| REPLACEME

@docs replaceMe

-}


type alias Interface a comparable =
    { op : a -> (comparable -> comparable -> Bool) -> a -> Bool
    , compare : a -> a -> Basics.Order
    , greater : a -> a -> a
    , lesser : a -> a -> a
    , greatest : List a -> Maybe a
    , least : List a -> Maybe a
    }


{-| REPLACEME
-}
makeInterface : (a -> comparable) -> Interface a comparable
makeInterface toComparable =
    { op = op toComparable
    , compare = compare toComparable
    , greater = greater toComparable
    , lesser = lesser toComparable
    , greatest = greatest toComparable
    , least = least toComparable
    }


op : (a -> comparable) -> a -> (comparable -> comparable -> Bool) -> a -> Bool
op toComparable arg1 operator arg2 =
    operator (toComparable arg2) (toComparable arg1)


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
