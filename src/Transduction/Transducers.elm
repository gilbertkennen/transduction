module Transduction.Transducers
    exposing
        ( transduce
        , compose
        , last
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
        , fold
        , mapInput
        , mapOutput
        , withDefault
        )

{-| Actual `Transducer` implementations.

@docs transduce, compose


# Transducers

@docs last, mapInput, mapOutput, fold, concat, take, repeatedly, reverse, filter, drop, intersperse, isEmpty, length, member, partition, repeat, withDefault

-}

import Transduction
    exposing
        ( Reducer
        , Transducer
        , transducer
        , simpleTransducer
        , emit
        , finish
        , finishWith
        , halt
        , unit
        , isHalted
        , reduce
        )
import Transduction.List.Shared as TList


{-| Actually apply your transducer. Automatically composes `last` to the end.
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


{-| An example of a transducer which doesn't care about what reducer it receives. Effectively a `Reducer`, but not technically.
-}
last : Transducer Never never input (Maybe input)
last =
    lastHelper Nothing


lastHelper : Maybe input -> Transducer Never never input (Maybe input)
lastHelper input =
    transducer
        (lastHelper << Just)
        (\_ -> input)


{-| Given a function to apply the elements of a collection to a `Reducer`, applies the elements of each collection ingested to the `Reducer`.
-}
concat :
    (collection -> Reducer input output -> Reducer input output)
    -> Transducer input output collection output
concat stepper =
    simpleTransducer
        (\xs reducer ->
            concat stepper (stepper xs reducer)
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
                (mapInput (\x -> [ padding, x ]) |> compose (concat TList.stepper))
                x
                reducer
        )


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
    partitionHelper predicate (compose last trueReducer unit) (compose last falseReducer unit)


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
