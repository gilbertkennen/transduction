module Transduction
    exposing
        ( Transducer
        , Reducer
        , transduce
        , reduce
        , compose
        , isHalted
        , halt
        , transducer
        , simpleTransducer
        , last
        , unit
        , finish
        , finishWith
        , emit
        , concat
        )

{-| Transducers are composable structures which process elements one at a time.


# Types

@docs Reducer, Transducer


# Functions

@docs compose, transduce


# Construction

Functions from this section should not be required by end-users.

@docs transducer, simpleTransducer, reduce, emit, last, finish, finishWith, halt, isHalted, concat, unit

-}


{-| You can't make your own base reducers, just wrap functions around those contained within.

All reducers are based upon a base reducer which returns `Nothing` if no value has been consumed or halts with `Just x` the first time it receives a value.

-}
type Reducer input output
    = Reducer (Maybe (input -> Reducer input output)) (() -> output)


{-| A `Transducer` is a function which wraps a `Reducer` producing a new `Reducer`.
-}
type alias Transducer afterInput afterOutput thisInput thisOutput =
    Reducer afterInput afterOutput -> Reducer thisInput thisOutput


{-| Transform a `Transducer` into a regular function after applying the base `Reducer`.
-}
transduce : Transducer afterInput (Maybe afterInput) thisInput thisOutput -> thisInput -> thisOutput
transduce trans x =
    finishWith x (compose last trans unit)


{-| Composes two transducers together. The parameter order is to make chaining using `|>` easier.

`first |> compose second |> compose third`

-}
compose :
    Transducer afterInput afterOutput middleInput middleOutput
    -> Transducer middleInput middleOutput thisInput thisOutput
    -> Transducer afterInput afterOutput thisInput thisOutput
compose =
    (>>)


{-| A basic `Reducer` which is halted and outputs `()`
-}
unit : Reducer a ()
unit =
    halt ()


{-| A basic transducer which always continues with the last value ingested.
-}
last : Transducer Never never input (Maybe input)
last =
    capHelper Nothing


capHelper : Maybe input -> Transducer Never never input (Maybe input)
capHelper input =
    transducer
        (capHelper << Just)
        (\_ -> input)


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
finish (Reducer _ finishF) =
    finishF ()


{-| It's very common to want to reduce one more value before finishing.
-}
finishWith : input -> Reducer input output -> output
finishWith x reducer =
    reduce x reducer |> finish


{-| Emit a value mapping the reply.
-}
emit :
    Transducer afterInput afterOutput thisInput thisOutput
    -> afterInput
    -> Reducer afterInput afterOutput
    -> Reducer thisInput thisOutput
emit trans x reducer =
    trans (reduce x reducer)


{-| Produce a `Reducer` which is in a halted state.
-}
halt : output -> Reducer input output
halt x =
    Reducer Nothing (\() -> x)


{-| Checks if the `Reducer` is in a halted state.
-}
isHalted : Reducer input output -> Bool
isHalted (Reducer reducerF _) =
    reducerF == Nothing


{-| Make a transducer.
-}
transducer :
    (thisInput -> Reducer afterInput afterOutput -> Reducer thisInput thisOutput)
    -> (Reducer afterInput afterOutput -> thisOutput)
    -> Transducer afterInput afterOutput thisInput thisOutput
transducer mapReduce mapFinish reducer =
    Reducer
        (Just (flip mapReduce reducer))
        (\() -> mapFinish reducer)


{-| Make a simple transducer which doesn't do anything fancy on finish.
-}
simpleTransducer :
    (thisInput -> Reducer afterInput output -> Reducer thisInput output)
    -> Transducer afterInput output thisInput output
simpleTransducer f ((Reducer _ finishF) as reducer) =
    Reducer
        (Just (flip f reducer))
        finishF


{-| Given a function to apply the elements of a collection to a `Reducer`, applies the elements of each collection ingested to the `Reducer`.
-}
concat :
    (Reducer input output -> collection -> Reducer input output)
    -> Transducer input output collection output
concat stepper =
    simpleTransducer
        (\xs reducer ->
            concat stepper (stepper reducer xs)
        )
