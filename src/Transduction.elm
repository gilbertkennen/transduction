module Transduction
    exposing
        ( Transducer(Transducer)
        , Reducer
        , Emitter
        , reduce
        , stepper
        , reducer
        , transducer
        , compose
        , extract
        )

{-| An Elm experiment in transducers. The purpose of transducers is to create composable elements which work on collections in powerful ways.

Transducers defined here will always try to do as much as possible to reduce the number of elements taken from the collection.


# Types

@docs Transducer, Reducer, Emitter


# Basic Transducer Functions

@docs reduce, transducer, reducer, compose


# Advanced Transducer Functions

@docs stepper, extract

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


{-| An `Emitter` is a special `Transducer` which only takes `()` as its input. The idea is that this would normally be the first `Transducer` in a stack and this is how one injects a collection to be reduced.
-}
type alias Emitter afterState afterInput afterResult thisState thisResult =
    Transducer afterState afterInput afterResult thisState () thisResult


{-| A `Reducer` is just a `Transducer` which effectively doesn't interact with anything after it. Defining it this way makes composition easier, just use `compose`.
-}
type alias Reducer state input result =
    Transducer () () () state input result


{-| When a `Transducer` has both an `Emitter` and a `Reducer`, it is complete and can be reduced.
-}
type alias Reduction state result =
    Transducer () () () state () result


{-| Where the magic happens. Takes a `Stepper` and a `Reducer` to make a function which reduces the collection.
-}
reduce : Reduction state result -> result
reduce reduction =
    let
        ( init, step, finish ) =
            extract reduction
    in
        doSteps step init |> Reply.state |> finish


{-| Execute just the stepping step of reduction.
-}
stepper : Emitter state input state emitterState state -> (input -> state -> Reply state) -> Reply state -> Reply state
stepper emitter stepF state =
    let
        fitting : Reducer state input state
        fitting =
            reducer
                state
                stepF
                identity

        ( init, step, finish ) =
            extract (compose fitting emitter)
    in
        doSteps step init |> Reply.map finish |> Reply.refill


doSteps : (() -> state -> Reply state) -> Reply state -> Reply state
doSteps step state =
    if Reply.isGo state then
        doSteps step (Reply.andThenContinue (step ()) state)
    else
        state


{-| Make your own `Transducer`. Takes three functions.

  - The first transforms a reducer's initial value.
  - The second transforms a reducer's step function.
  - The third transforms a reducer's finish function.

This is an alternative to using the constructor which reduces some minor flexibility for the majority of cases.

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


{-| Get the raw functions of a transducer. Occasionally helpful to construct particularly tricky transducers.
-}
extract : Reducer state input result -> TransducerTriple state input result
extract (Transducer reducer) =
    let
        unit : TransducerTriple () () ()
        unit =
            ( (Reply.continue ())
            , (\() () -> Reply.continue ())
            , (\() -> ())
            )
    in
        reducer unit
