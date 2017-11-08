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
        , cap
        , finish
        , finishWith
        , emit
        , fold
        , mapInput
        , mapOutput
        , withDefault
        , concat
        , take
        , repeatedly
        , reverse
        , filter
        , drop
        , intersperse
        , isEmpty
        , length
        , member
        , partition
        , repeat
        )

{-| Transducers are composable structures which process elements one at a time.


# Types

@docs Reducer, Transducer


# Functions

@docs compose, transduce


# Construction

Functions from this section should not be required by end-users.

@docs transducer, simpleTransducer, reduce, emit, cap, finish, finishWith, halt, isHalted


# Transducers

@docs mapInput, mapOutput, fold, concat, take, repeatedly, reverse, filter, drop, intersperse, isEmpty, length, member, partition, repeat, withDefault

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
    finishWith x (trans cap)


{-| Composes two transducers together. The parameter order is to make chaining using `|>` easier.

`first |> compose second |> compose third`

-}
compose :
    Transducer afterInput afterOutput middleInput middleOutput
    -> Transducer middleInput middleOutput thisInput thisOutput
    -> Transducer afterInput afterOutput thisInput thisOutput
compose =
    (>>)


unit : Reducer a ()
unit =
    Reducer Nothing identity


{-| A basic reducer which always continues with the last value ingested.
-}
cap : Reducer input (Maybe input)
cap =
    capHelper Nothing


capHelper : Maybe input -> Reducer input (Maybe input)
capHelper input =
    Reducer
        (Just (capHelper << Just))
        (\() -> input)


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


{-| Maps inputs.
-}
mapInput : (thisInput -> afterInput) -> Transducer afterInput output thisInput output
mapInput f =
    simpleTransducer
        (\x reducer ->
            emit (mapInput f) (f x) reducer
        )


{-| Each element updates the state which emits on finish.
-}
fold :
    (input -> state -> state)
    -> state
    -> Transducer state output input output
fold step state =
    transducer
        (\x reducer ->
            fold step (step x state) reducer
        )
        (\reducer -> finishWith state reducer)


{-| Upon ingesting an input, just keeps emitting that input until a `Halt` reply is received.
This `Transducer` is potentially infinite, so make sure that whatever is passed will eventually `Halt` on its own.
-}
repeatedly : Transducer input output input output
repeatedly =
    simpleTransducer
        (\input reducer ->
            if isHalted reducer then
                reducer
            else
                reduce input (repeatedly (reduce input reducer))
        )


{-| If `n <= 0`, then `Halt` without emitting. Otherwise, pass through until `n` elements have been emitted.
-}
take : Int -> Transducer input output input output
take n =
    simpleTransducer
        (\x reducer ->
            if n <= 0 then
                finish reducer |> halt
            else if n == 1 then
                finishWith x reducer |> halt
            else
                emit (take (n - 1)) x reducer
        )


{-| On finish, emits a list of elements received in reverse order.
-}
reverse : Transducer (List input) output input output
reverse =
    reverseHelper []


reverseHelper : List input -> Transducer (List input) output input output
reverseHelper state =
    transducer
        (\x reducer ->
            reverseHelper (x :: state) reducer
        )
        (\reducer ->
            finishWith state reducer
        )


{-| Emits values where predicate is true.
-}
filter : (input -> Bool) -> Transducer input output input output
filter predicate =
    simpleTransducer
        (\x reducer ->
            if predicate x then
                emit (filter predicate) x reducer
            else
                filter predicate reducer
        )


{-| Ignore the first n elements and then emit everything else.
-}
drop : Int -> Transducer input output input output
drop n =
    simpleTransducer
        (\x reducer ->
            if n <= 0 then
                emit identity x reducer
            else
                drop (n - 1) reducer
        )


{-| Emits the padding value before each value after the first.
-}
intersperse : input -> Transducer input output input output
intersperse padding =
    simpleTransducer
        (\x reducer ->
            emit
                (mapInput (\x -> [ padding, x ]) |> compose (concat listStepper))
                x
                reducer
        )


listStepper : Reducer input output -> List input -> Reducer input output
listStepper reducer xs =
    case xs of
        [] ->
            reducer

        x :: rest ->
            if isHalted reducer then
                reducer
            else
                listStepper (reduce x reducer) rest


{-| Emits `False` and `Halt`s if it receives an element. Emits `True` on finish.
-}
isEmpty : Transducer Bool output input output
isEmpty =
    transducer
        (\x reducer ->
            finishWith False reducer |> halt
        )
        (\reducer ->
            finishWith True reducer
        )


{-| Emits the number of elements seen on finish.
-}
length : Transducer Int output input output
length =
    lengthHelper 0


{-| Emits a count of elements seen on finish.
-}
lengthHelper : Int -> Transducer Int output input output
lengthHelper count =
    transducer
        (\x reducer ->
            lengthHelper (count + 1) reducer
        )
        (\reducer ->
            finishWith count reducer
        )


{-| Emits a `Bool` indicating whether the value has been seen on finish.
-}
member : input -> Transducer Bool output input output
member comp =
    transducer
        (\x reducer ->
            if x == comp then
                finishWith True reducer |> halt
            else
                member comp reducer
        )
        (\reducer -> finishWith False reducer)


{-| "Applies one of two different reducers depending on the predicate. Emits a tuple of the reducers output on finish.."
-}
partition :
    (input -> Bool)
    -> Transducer trueInput (Maybe trueInput) input trueOutput
    -> Transducer falseInput (Maybe falseInput) input falseOutput
    -> Transducer ( trueOutput, falseOutput ) output input output
partition predicate trueReducer falseReducer =
    partitionHelper predicate (trueReducer cap) (falseReducer cap)


partitionHelper :
    (input -> Bool)
    -> Reducer input trueOutput
    -> Reducer input falseOutput
    -> Transducer ( trueOutput, falseOutput ) output input output
partitionHelper predicate trueReply falseReply =
    transducer
        (\x reducer ->
            if predicate x then
                partitionHelper predicate (reduce x trueReply) falseReply reducer
            else
                partitionHelper predicate trueReply (reduce x falseReply) reducer
        )
        (\reducer ->
            finishWith ( finish trueReply, finish falseReply ) reducer
        )


{-| Upon ingesting the tuple, emits the value n times.
-}
repeat : Transducer input output ( Int, input ) output
repeat =
    simpleTransducer
        (\( n, x ) reducer ->
            repeat (doRepeat n x reducer)
        )


doRepeat : Int -> input -> Reducer input output -> Reducer input output
doRepeat n x reducer =
    if n <= 0 || isHalted reducer then
        reducer
    else
        doRepeat (n - 1) x (reduce x reducer)


{-| Map the transducer output value.
-}
mapOutput : (afterOutput -> thisOutput) -> Transducer input afterOutput input thisOutput
mapOutput f =
    transducer
        (\x reducer ->
            emit (mapOutput f) x reducer
        )
        (f << finish)


{-| Provide the default value if output is `Nothing`.
-}
withDefault : output -> Transducer input (Maybe output) input output
withDefault value =
    mapOutput (Maybe.withDefault value)
