module Transduction
    exposing
        ( Reducer(Reducer)
        , Transducer
        , Stepper
        , reduce
        , map
        , statefulMap
        , take
        , withIndex
        )

{-| An Elm experiment in transducers. The purpose of transducers is to create composable elements which work on collections in powerful ways.
-}

import Transduction.Reply as Reply exposing (Reply)


{-| The `reduce` function needs a `Reducer`. This is a triple of:

  - Initial `Reply` state (can be `Halt` to stop before you begin.)
  - A step function which updates the state based on an element from the collection.
  - A final clean-up step.

-}
type Reducer state result a
    = Reducer (Reply state) (a -> state -> Reply state) (state -> result)


{-| The titular data structure is just a function which wraps itself around a `Reducer`. Transducers compose like normal functions using `(<<)` and `(>>)`. Note that the direction of the arrows is the **opposite** of the flow of collection values.

The type variables are arranged as pairs of state, result, and values. The left of the pair is the type expected to be inserted and the right of the pair is the type handed to the `Reducer` that is wrapped.

-}
type alias Transducer state1 state2 result1 result2 a b =
    Reducer state2 result2 b -> Reducer state1 result1 a


{-| A stepper is a function which applies the step function successively to each element of the collection. This could be trivially implemented using `foldl`, but this gives the flexibility of implementing early termination based on the `Reply`.
-}
type alias Stepper state collection a =
    (a -> state -> Reply state) -> Reply state -> collection -> Reply state


{-| Where the magic happens. Takes a `Stepper` and a `Reducer` to make a function which reduces the collection.
-}
reduce : Stepper state collection a -> Reducer state result a -> collection -> result
reduce stepper (Reducer init step finish) collection =
    stepper step init collection |> Reply.state |> finish


map : (a -> b) -> Transducer state state result result a b
map f (Reducer init step finish) =
    Reducer
        init
        (step << f)
        finish


statefulMap :
    thisState
    -> (a -> thisState -> Result thisState ( b, thisState ))
    -> Transducer ( thisState, thatState ) thatState result result a b
statefulMap init1 step1 (Reducer init2 step2 finish2) =
    Reducer
        (Reply.map ((,) init1) init2)
        (\x ( state1, state2 ) ->
            case step1 x state1 of
                Err newState1 ->
                    Reply.halt ( newState1, state2 )

                Ok ( newX, newState1 ) ->
                    Reply.map ((,) newState1) (step2 newX state2)
        )
        (finish2 << Tuple.second)


withIndex : Transducer ( Int, state ) state result result a ( Int, a )
withIndex =
    statefulMap 0 (\x n -> Ok ( ( n, x ), n + 1 ))


take : Int -> Transducer ( Int, state ) state result result a a
take n (Reducer init step finish) =
    Reducer
        (Reply.andThen
            (\state ->
                if n <= 0 then
                    Reply.halt ( n, state )
                else
                    Reply.continue ( n, state )
            )
            init
        )
        (\x ( m, state ) ->
            Reply.andThen
                (\newState ->
                    if m <= 1 then
                        Reply.halt ( m - 1, newState )
                    else
                        Reply.continue ( m - 1, newState )
                )
                (step x state)
        )
        (finish << Tuple.second)



-- andThen : Stepper state collection b -> state -> Transducer state state result result collection b
-- andThen stepper init =
