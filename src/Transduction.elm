module Transduction
    exposing
        ( Reducer
        , Transducer
        , Stepper
        , reduce
        , reducer
        , transducer
        , compose
        , map
        , statefulMap
        , take
        , drop
        , withIndex
        , withCount
        , concat
        )

{-| An Elm experiment in transducers. The purpose of transducers is to create composable elements which work on collections in powerful ways.

Transducers defined here will always try to do as much as possible to reduce the number of elements taken from the collection.


# Types

@docs Transducer, Reducer, Stepper


# Basic Transducer Functions

@docs reduce, transducer, reducer, compose


# Transducers

@docs map, statefulMap, take, drop, withIndex, withCount, concat

-}

import Transduction.Reply as Reply exposing (Reply)


{-| The titular data structure. These can be composed together in chains.
-}
type Transducer afterState afterInput afterResult thisState thisInput thisResult
    = Transducer
        (TransducerTriple afterState afterInput afterResult
         -> TransducerTriple thisState thisInput thisResult
        )


type alias TransducerTriple state input result =
    ( Reply state
    , input
      -> state
      -> Reply state
    , state -> result
    )


{-| A `Reducer` is just a `Transducer` which effectively doesn't interact with anything after it. Defining it this way makes composition easier, just use `compose`.
-}
type alias Reducer state input result =
    Transducer () () () state input result


{-| A stepper is a function which applies the step function successively to each element of the collection. This could be trivially implemented using `foldl`, but this gives the flexibility of implementing early termination based on the `Reply`.
-}
type alias Stepper state collection element =
    (element -> state -> Reply state) -> Reply state -> collection -> Reply state


{-| Where the magic happens. Takes a `Stepper` and a `Reducer` to make a function which reduces the collection.
-}
reduce : Stepper state collection input -> Reducer state input result -> collection -> result
reduce stepper (Transducer reducer) collection =
    let
        unit : TransducerTriple () () ()
        unit =
            ( (Reply.continue ())
            , (\() () -> Reply.continue ())
            , (\() -> ())
            )

        ( init, step, finish ) =
            reducer unit
    in
        stepper step init collection |> Reply.state |> finish


{-| Make your own `Transducer`. Takes three functions.

  - The first transforms a reducer's initial value.
  - The second transforms a reducer's step function.
  - The third transforms a reducer's finish function.

-}
transducer :
    (Reply afterState -> Reply thisState)
    -> ((afterInput -> afterState -> Reply afterState) -> thisInput -> thisState -> Reply thisState)
    -> ((afterState -> afterResult) -> thisState -> thisResult)
    -> Transducer afterState afterInput afterResult thisState thisInput thisResult
transducer initF stepF finishF =
    Transducer (\( init, step, finish ) -> ( initF init, stepF step, finishF finish ))


{-| Make your own `Reducer`. Composed of:

  - Initial `Reply` state (can be `Halt` to stop before you begin).
  - A step function which updates the state based on an element from the collection.
  - A final clean-up step.

-}
reducer :
    Reply state
    -> (input -> state -> Reply state)
    -> (state -> result)
    -> Reducer state input result
reducer init step finish =
    Transducer (\_ -> ( init, step, finish ))


{-| If you have two transducers you can merge them into one.

Don't let the long type definition intimidate you, there is nothing tricky going on. The order of the two parameters is the reverse order of the final `Transducer` and the types need to just line up given that ordering.

The parameters are in this order to make `|>` chaining easier. `foo |> compose bar |> compose baz` produces a `Transducer` in the order foo, bar, baz.

-}
compose :
    Transducer afterState afterInput afterResult betweenState betweenInput betweenResult
    -> Transducer betweenState betweenInput betweenResult thisState thisInput thisResult
    -> Transducer afterState afterInput afterResult thisState thisInput thisResult
compose (Transducer transducer1) (Transducer transducer2) =
    Transducer (transducer1 >> transducer2)


{-| Maps the collection's element values, not the result.
-}
map : (thisInput -> afterInput) -> Transducer state afterInput result state thisInput result
map f =
    transducer identity ((>>) f) identity


{-| Sometimes you need to remember a bit of state while mapping.
-}
statefulMap :
    thisState
    -> (thisInput -> thisState -> ( afterInput, thisState ))
    -> Transducer afterState afterInput result ( thisState, afterState ) thisInput result
statefulMap init1 step1 =
    transducer
        (\init2 -> Reply.map ((,) init1) init2)
        (\step2 ->
            (\x ( state1, state2 ) ->
                step1 x state1
                    |> (\( newX, newState1 ) ->
                            Reply.map ((,) newState1) (step2 newX state2)
                       )
            )
        )
        ((>>) Tuple.second)


{-| Include an index with each element.
-}
withIndex : Transducer afterState ( Int, thisInput ) result ( Int, afterState ) thisInput result
withIndex =
    statefulMap 0 (\x n -> ( ( n, x ), n + 1 ))


{-| Stop the iteration after the specified number of elements.
-}
take : Int -> Transducer afterState input result ( Int, afterState ) input result
take n =
    transducer
        (\init ->
            Reply.andThen
                (\state ->
                    if n <= 0 then
                        Reply.halt ( n, state )
                    else
                        Reply.continue ( n, state )
                )
                init
        )
        (\step x ( m, state ) ->
            Reply.andThen
                (\newState ->
                    if m <= 1 then
                        Reply.halt ( m - 1, newState )
                    else
                        Reply.continue ( m - 1, newState )
                )
                (step x state)
        )
        ((>>) Tuple.second)


{-| Skip the first n elements. Negatives count as 0.
-}
drop : Int -> Transducer afterState input result ( Int, afterState ) input result
drop n =
    transducer
        (Reply.map ((,) n))
        (\step x ( m, state ) ->
            if m <= 0 then
                Reply.map ((,) m) (step x state)
            else
                Reply.continue ( m - 1, state )
        )
        ((>>) Tuple.second)


{-| Attach a count of elements to the final result.
-}
withCount : Transducer afterState input afterResult ( Int, afterState ) input ( Int, afterResult )
withCount =
    transducer
        (Reply.map ((,) 0))
        (\step x ( n, state ) -> Reply.map ((,) (n + 1)) (step x state))
        Tuple.mapSecond


{-| Given an appropriate `Stepper` function, deconstruct each collection passed in and pass elements down instead.
-}
concat :
    Stepper state collection afterInput
    -> Transducer state afterInput result state collection result
concat stepper =
    transducer
        identity
        (\step collection state ->
            stepper step (Reply.continue state) collection
        )
        identity
