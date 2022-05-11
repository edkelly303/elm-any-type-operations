module Arithmetic exposing
    ( Config
    , Interface
    , Op
    , add
    , div
    , div0
    , divM
    , makeFloatInterface
    , makeIntegerInterface
    , makeInterface
    , mul
    , sub
    )


type alias Interface a =
    a -> Op -> a -> a


type alias Config num =
    { add : num -> num -> num
    , sub : num -> num -> num
    , mul : num -> num -> num
    , div : num -> num -> num
    , div0 : num -> num -> num
    , divM : num -> num -> num
    }


type Op
    = Add
    | Sub
    | Mul
    | Div
    | Div0
    | DivM


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


div0 : Op
div0 =
    Div0


divM : Op
divM =
    DivM


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
        , div0 = if arg2 == 0 then fromInt 0 else calc (//)
        , divM = if arg2 == 0 then fromInt minInt else calc (//)
        }


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
        , div0 = if arg2 == 0.0 then fromInt 0.0 else calc (/)
        , divM = if arg2 == 0.0 then fromInt minInt else calc (/)
        }


makeInterface : Config a -> Interface a
makeInterface config =
    op config


op : Config a -> a -> Op -> a -> a
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


minInt : Int
minInt = -2147483648
