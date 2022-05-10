module Arithmetic exposing
    ( Interface
    , Op(..)
    , add
    , div
    , makeFloatInterface
    , makeIntegerInterface
    , makeInterface
    , mul
    , sub
    )


type alias Interface a =
    { op : a -> Op -> a -> a
    }


type alias Config a num =
    { toNum : a -> num
    , fromNum : num -> a
    , add : num -> num -> num
    , sub : num -> num -> num
    , mul : num -> num -> num
    , div : num -> num -> num
    }


type Op
    = Add
    | Sub
    | Mul
    | Div


add : Op
add =
    Add


sub : Op
sub =
    Sub


mul : Op
mul =
    Mul


div : Op
div =
    Div


makeIntegerInterface : { toInt : a -> Int, fromInt : Int -> a } -> Interface a
makeIntegerInterface { toInt, fromInt } =
    makeInterface
        { toNum = toInt
        , fromNum = fromInt
        , add = (+)
        , sub = (-)
        , mul = (*)
        , div = (//)
        }


makeFloatInterface : { toFloat : a -> Float, fromFloat : Float -> a } -> Interface a
makeFloatInterface { toFloat, fromFloat } =
    makeInterface
        { toNum = toFloat
        , fromNum = fromFloat
        , add = (+)
        , sub = (-)
        , mul = (*)
        , div = (/)
        }


makeInterface : Config a num -> Interface a
makeInterface config =
    { op = op config }


op : Config a num -> a -> Op -> a -> a
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
    opFn (c.toNum arg1) (c.toNum arg2)
        |> c.fromNum
