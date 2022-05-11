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
        , div0 = \arg1 arg2 -> 
            if toInt arg2 == 0 then 
                fromInt 0 
            else 
                calc (//) arg1 arg2
        , divM = \arg1 arg2 -> 
            if toInt arg2 == 0 then 
                fromInt lowestNumber 
            else 
                calc (//) arg1 arg2
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
        , div0 = \arg1 arg2 -> 
            if toFloat arg2 == 0.0 then 
                fromFloat 0.0 
            else 
                calc (/) arg1 arg2
        , divM = \arg1 arg2 -> 
            if toFloat arg2 == 0.0 then 
                fromFloat lowestNumber 
            else 
                calc (/) arg1 arg2
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


lowestNumber : number
lowestNumber = -2147483648
