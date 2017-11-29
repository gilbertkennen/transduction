module Transduction
    exposing
        ( Transducer
        , Reducer
        , reduce
        , isHalted
        , halt
        , transducer
        , forcedTransducer
        , simpleTransducer
        , advancedTransducer
        , unit
        , finish
        , finishWith
        , emit
        )

{-| Transducers are composable structures which process elements one at a time.


# Types

@docs Reducer, Transducer


# Construction

Functions from this section should not be required by end-users.

@docs transducer, forcedTransducer, simpleTransducer, advancedTransducer, reduce, emit, finish, finishWith, halt, isHalted, unit

-}

import Lazy exposing (Lazy, lazy, force)


{-| You can't make your own base reducers, just wrap functions around those contained within.

All reducers are based upon a base reducer which returns `Nothing` if no value has been consumed or halts with `Just x` the first time it receives a value.

-}
type Reducer input output
    = Reducer (Maybe (input -> Reducer input output)) (Lazy output)


{-| A `Transducer` is a function which wraps a `Reducer` producing a new `Reducer`.
-}
type alias Transducer reducerInput reducerOutput thisInput thisOutput =
    Reducer reducerInput reducerOutput -> Reducer thisInput thisOutput


{-| A basic `Reducer` which is halted and outputs `()`
-}
unit : Reducer a ()
unit =
    halt ()


{-| Apply the reducer to an input value.
-}
reduce : input -> Reducer input output -> Reducer input output
reduce x ((Reducer reduceF _) as reducer) =
    case reduceF of
        Nothing ->
            reducer

        Just f ->
            f x


{-| Calculate the finished value of a `Reducer`.
-}
finish : Reducer input output -> output
finish (Reducer _ finish) =
    force finish


{-| It's very common to want to reduce one more value before finishing.
-}
finishWith : input -> Reducer input output -> output
finishWith x reducer =
    reduce x reducer |> finish


{-| Emit a value mapping the reply.
-}
emit :
    Transducer reducerInput reducerOutput thisInput thisOutput
    -> reducerInput
    -> Reducer reducerInput reducerOutput
    -> Reducer thisInput thisOutput
emit trans x reducer =
    trans (reduce x reducer)


{-| Produce a `Reducer` which is in a halted state.
-}
halt : output -> Reducer input output
halt x =
    Reducer Nothing (lazy (\() -> x))


{-| Checks if the `Reducer` is in a halted state.
-}
isHalted : Reducer input output -> Bool
isHalted (Reducer reducerF _) =
    reducerF == Nothing


{-| Make a transducer.

Checks if the `Reducer` is halted and if so, simply halts. This prevents the first element from emitting if nothing is going to happen.

-}
transducer :
    (thisInput -> Reducer reducerInput reducerOutput -> Reducer thisInput thisOutput)
    -> (Reducer reducerInput reducerOutput -> thisOutput)
    -> Transducer reducerInput reducerOutput thisInput thisOutput
transducer mapReduce mapFinish reducer =
    case reducer of
        Reducer Nothing _ ->
            Reducer Nothing (lazy (\() -> mapFinish reducer))

        _ ->
            Reducer
                (Just (flip mapReduce reducer))
                (lazy (\() -> mapFinish reducer))


{-| Make a transducer.

Always wraps the `Reducer` even if it is halted. This is good for when your `Transducer` has useful effects even if there is nothing to emit to.

-}
forcedTransducer :
    (thisInput -> Reducer reducerInput reducerOutput -> Reducer thisInput thisOutput)
    -> (Reducer reducerInput reducerOutput -> thisOutput)
    -> Transducer reducerInput reducerOutput thisInput thisOutput
forcedTransducer mapReduce mapFinish reducer =
    Reducer
        (Just (flip mapReduce reducer))
        (lazy (\() -> mapFinish reducer))


{-| Make a simple transducer which doesn't do anything fancy on finish.
-}
simpleTransducer :
    (thisInput -> Reducer reducerInput output -> Reducer thisInput output)
    -> Transducer reducerInput output thisInput output
simpleTransducer f ((Reducer _ finishF) as reducer) =
    Reducer
        (Just (flip f reducer))
        finishF


{-| For when you want all the control.

Only needed if you want to terminate early based on initial state.

-}
advancedTransducer :
    Maybe (Reducer reducerInput reducerOutput -> Maybe (thisInput -> Reducer thisInput thisOutput))
    -> (Reducer reducerInput reducerOutput -> thisOutput)
    -> Transducer reducerInput reducerOutput thisInput thisOutput
advancedTransducer maybeMakeReducerF mapFinish reducer =
    Reducer
        (Maybe.andThen ((|>) reducer) maybeMakeReducerF)
        (lazy (\() -> mapFinish reducer))
