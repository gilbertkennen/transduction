module Transduction
    exposing
        ( Transducer
        , Reducer
        , Reply(Continue, Halt)
        , compose
        , cap
        , finish
        , mapReply
        , apply
        , emit
        , fold
        , mapInput
        , concat
        , take
        , repeatedly
        , reverse
        , filter
        , drop
        , intersperse
        , isEmpty
        , length
        , emitter
        , member
        , partition
        , repeat
        )

{-| Transducers are composable structures which process elements one at a time.


# Types

@docs Reducer, Reply, Transducer


# Functions

@docs compose, cap, finish


# Construction

@docs mapReply, apply, emit


# Transducers

@docs mapInput, fold, concat, take, repeatedly, reverse, filter, drop, intersperse, isEmpty, length, emitter, member, partition, repeat

-}


{-| A `Reducer` either ingests an element or an indication that further elements won't be coming and produce a `Reply` (explained below).
-}
type alias Reducer input output =
    Maybe input -> Reply input output


{-| A `Reply` either contains the end output of a `Reducer` or a new `Reducer` ready to ingest another element.
-}
type Reply input output
    = Continue (Reducer input output)
    | Halt output


{-| A `Transducer` is a function which wraps a `Reducer` producing a new `Reducer`.
-}
type alias Transducer afterInput afterOutput thisInput thisOutput =
    Reducer afterInput afterOutput -> Reducer thisInput thisOutput


{-| Composes two transducers together. The parameter order is to make chaining using `|>` easier.

`first |> compose second |> compose third`

-}
compose :
    Transducer afterInput afterOutput middleInput middleOutput
    -> Transducer middleInput middleOutput thisInput thisOutput
    -> Transducer afterInput afterOutput thisInput thisOutput
compose =
    (>>)


{-| A most simple `Reducer` which `Halt`s if it receives a value and continues if it doesn't. Useful for turning a `Transducer` into a `Reducer`.
-}
cap : Reducer input input
cap maybeX =
    case maybeX of
        Nothing ->
            Continue cap

        Just x ->
            Halt x


{-| Hopefully get an output from a reply by passing `Nothing` in.
-}
finish : Reply input output -> output
finish reply =
    case reply of
        Halt output ->
            output

        Continue reducer ->
            finish (repeatedly reducer Nothing)


{-| Map the values in a `Reply`, in case the `Transducer` wants to be `Reply` agnostic.
-}
mapReply :
    Transducer afterInput afterOutput thisInput thisOutput
    -> (afterOutput -> thisOutput)
    -> Reply afterInput afterOutput
    -> Reply thisInput thisOutput
mapReply transducer finish reply =
    case reply of
        Halt x ->
            Halt (finish x)

        Continue reducer ->
            Continue (transducer reducer)


{-| Emit a value mapping the reply.
-}
emit :
    Transducer afterInput afterOutput thisInput thisOutput
    -> (afterOutput -> thisOutput)
    -> Reducer afterInput afterOutput
    -> Maybe afterInput
    -> Reply thisInput thisOutput
emit transducer outputMap reducer maybeX =
    reducer maybeX |> mapReply transducer outputMap


{-| Applies a `Continue` reducer to the input if possible.
-}
apply : Maybe input -> Reply input output -> Reply input output
apply maybeX reply =
    case reply of
        Halt _ ->
            reply

        Continue reducer ->
            reducer maybeX


{-| Given a function to apply the elements of a collection to a `Reducer`, applies the elements of each collection ingested to the `Reducer`.
-}
concat : (Reducer input output -> collection -> Reply input output) -> Transducer input output collection output
concat stepper reducer maybeX =
    case maybeX of
        Nothing ->
            mapReply (concat stepper) identity (reducer Nothing)

        Just xs ->
            stepper reducer xs
                |> mapReply (concat stepper) identity


{-| Maps inputs.
-}
mapInput : (thisInput -> afterInput) -> Transducer afterInput output thisInput output
mapInput f reducer maybeX =
    mapReply (mapInput f) identity (reducer (Maybe.map f maybeX))


{-| Applies a standard sort of fold step and emits its state upon ingesting `Nothing`.
-}
fold :
    (input -> state -> state)
    -> state
    -> Transducer state output input output
fold step state reducer maybeX =
    case maybeX of
        Nothing ->
            mapReply (fold step state) identity (reducer (Just state))

        Just x ->
            Continue (fold step (step x state) reducer)


{-| Upon ingesting an input, just keeps emitting that input until a `Halt` reply is received.

This `Transducer` is potentially infinite, so make sure that whatever is passed will eventually `Halt` on its own.

-}
repeatedly : Transducer afterInput output afterInput output
repeatedly reducer input =
    case reducer input of
        Halt x ->
            Halt x

        Continue newReduction ->
            repeatedly newReduction input


{-| If `n <= 0`, then `Halt` without emitting. Otherwise, pass through until `n` elements have been emitted.
-}
take : Int -> Transducer input output input output
take n reducer maybeX =
    case maybeX of
        Nothing ->
            mapReply (take n) identity (reducer maybeX)

        Just _ ->
            if n <= 0 then
                Halt (finish (Continue reducer))
            else if n == 1 then
                Halt (finish (reducer maybeX))
            else
                mapReply (take (n - 1)) identity (reducer maybeX)


{-| On Nothing, emits a list of elements received in reverse order.
-}
reverse : Transducer (List input) output input output
reverse =
    reverseHelper []


reverseHelper : List input -> Transducer (List input) output input output
reverseHelper state reducer maybeX =
    case maybeX of
        Nothing ->
            mapReply reverse identity (reducer (Just state))

        Just x ->
            Continue (reverseHelper (x :: state) reducer)


{-| Emits values where predicate is true.
-}
filter : (input -> Bool) -> Transducer input output input output
filter predicate reducer maybeX =
    case maybeX of
        Nothing ->
            mapReply (filter predicate) identity (reducer Nothing)

        Just x ->
            if predicate x then
                mapReply (filter predicate) identity (reducer (Just x))
            else
                Continue (filter predicate reducer)


{-| Ignore the first n elements and then emit everything else.
-}
drop : Int -> Transducer input output input output
drop n reducer maybeX =
    case maybeX of
        Nothing ->
            mapReply (drop n) identity (reducer maybeX)

        Just _ ->
            if n <= 0 then
                mapReply identity identity (reducer maybeX)
            else
                Continue (drop (n - 1) reducer)


{-| Emits the padding value before each value after the first.
-}
intersperse : input -> Transducer input output input output
intersperse padding reducer maybeX =
    case maybeX of
        Nothing ->
            emit (intersperse padding) identity reducer maybeX

        Just _ ->
            emit
                (mapInput (\x -> [ padding, x ]) |> compose (concat listStepper))
                identity
                reducer
                maybeX


listStepper : Reducer input output -> List input -> Reply input output
listStepper reducer xs =
    case xs of
        [] ->
            Continue reducer

        x :: rest ->
            case reducer (Just x) of
                Halt output ->
                    Halt output

                Continue nextReducer ->
                    listStepper nextReducer rest


{-| Emits True on `Nothing` and False on `Just`.
-}
isEmpty : Transducer Bool output input output
isEmpty reducer maybeX =
    case maybeX of
        Nothing ->
            emit isEmpty identity reducer (Just True)

        Just _ ->
            emit (mapInput (always False)) identity reducer (Just False)


{-| Emits the number of elements seen upon ingesting `Nothing`.
-}
length : Transducer Int output input output
length =
    lengthHelper 0


{-| Emits a count of elements seen on `Nothing`
-}
lengthHelper : Int -> Transducer Int output input output
lengthHelper count reducer maybeX =
    case maybeX of
        Nothing ->
            emit (lengthHelper count) identity reducer (Just count)

        Just _ ->
            Continue (lengthHelper (count + 1) reducer)


{-| Emits provided value upon ingesting `Nothing`.
-}
emitter : afterInput -> Transducer afterInput output thisInput output
emitter value reducer maybeX =
    case maybeX of
        Nothing ->
            emit (emitter value) identity reducer (Just value)

        Just _ ->
            Continue (emitter value reducer)


{-| Upon ingesting `Nothing`, emits a `Bool` indicating whether the value has been seen.
-}
member : input -> Transducer Bool output input output
member comp reducer maybeX =
    case maybeX of
        Nothing ->
            emit (member comp) identity reducer (Just False)

        Just x ->
            if x == comp then
                Continue (emitter True reducer)
            else
                Continue (member comp reducer)


{-| "Applies one of two different reducers depending on the predicate. Emits a tuple of the reducers output upon ingesting `Nothing`."
-}
partition :
    (input -> Bool)
    -> Reducer input trueOutput
    -> Reducer input falseOutput
    -> Transducer ( trueOutput, falseOutput ) output input output
partition predicate trueReducer falseReducer =
    partitionHelper predicate (Continue trueReducer) (Continue falseReducer)


partitionHelper :
    (input -> Bool)
    -> Reply input trueOutput
    -> Reply input falseOutput
    -> Transducer ( trueOutput, falseOutput ) output input output
partitionHelper predicate trueReply falseReply reducer maybeX =
    case maybeX of
        Nothing ->
            emit
                (partitionHelper predicate trueReply falseReply)
                identity
                reducer
                (Just ( finish trueReply, finish falseReply ))

        Just x ->
            if predicate x then
                Continue (partitionHelper predicate (apply (Just x) trueReply) falseReply reducer)
            else
                Continue (partitionHelper predicate trueReply (apply (Just x) falseReply) reducer)


{-| Upon ingesting the tuple, emits the second command n times.
-}
repeat : Transducer input output ( Int, Maybe input ) output
repeat reducer maybeX =
    case maybeX of
        Nothing ->
            emit repeat identity reducer Nothing

        Just ( n, x ) ->
            doRepeat n x (Continue reducer)
                |> mapReply repeat identity


doRepeat : Int -> Maybe input -> Reply input output -> Reply input output
doRepeat n maybeX reply =
    if n <= 0 then
        reply
    else
        case reply of
            Halt _ ->
                reply

            Continue reducer ->
                doRepeat (n - 1) maybeX (reducer maybeX)
