module Transduction
    exposing
        ( Transducer
        , Reducer
        , Reply(Continue, Halt)
        , transduce
        , reduce
        , compose
          -- , mapReply
        , transducer
        , simpleTransducer
        , cap
        , finish
        , finishReply
        , finishWith
        , apply
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

@docs Reducer, Reply, Transducer


# Functions

@docs compose, transduce


# Construction

Functions from this section should not be required by end-users.

@docs transducer, simpleTransducer, reduce, apply, emit, cap, finish, finishReply, finishWith


# Transducers

@docs mapInput, mapOutput, fold, concat, take, repeatedly, reverse, filter, drop, intersperse, isEmpty, length, member, partition, repeat, withDefault

-}


{-| You can't make your own base reducers, just wrap functions around those contained within.

All reducers are based upon a base reducer which returns `Nothing` if no value has been consumed or halts with `Just x` the first time it receives a value.

-}
type Reducer input output
    = Reducer (input -> Reply input output) (() -> output)


{-| A `Reply` either contains the end output of a `Reducer` or a new `Reducer` ready to ingest another element.
-}
type Reply input output
    = Continue (Reducer input output)
    | Halt output


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


{-| The only basic reducer. Not sure if this should halt on receiving its first value or continue. Currently halts.
-}
cap : Reducer input (Maybe input)
cap =
    capHelper Nothing


capHelper : Maybe input -> Reducer input (Maybe input)
capHelper input =
    Reducer
        (\x -> Continue (capHelper (Just x)))
        (\() -> input)


{-| Apply the reducer to an input value.
-}
reduce : input -> Reducer input output -> Reply input output
reduce x (Reducer reduceF _) =
    reduceF x


{-| Calculate the finished value of a `Reducer`.
-}
finish : Reducer input output -> output
finish (Reducer _ finishF) =
    finishF ()


{-| Calculate the finished value of a `Reply`
-}
finishReply : Reply afterInput afterOutput -> afterOutput
finishReply reply =
    case reply of
        Halt x ->
            x

        Continue reducer ->
            finish reducer


{-| It's very common to want to reduce one more value before finishing.
-}
finishWith : input -> Reducer input output -> output
finishWith x reducer =
    reduce x reducer |> finishReply


{-| Map the values in a `Reply`, in case the `Transducer` wants to be `Reply` agnostic.
-}
mapReply :
    Transducer afterInput afterOutput thisInput thisOutput
    -> (afterOutput -> thisOutput)
    -> Reply afterInput afterOutput
    -> Reply thisInput thisOutput
mapReply trans finish reply =
    case reply of
        Halt x ->
            Halt (finish x)

        Continue reducer ->
            Continue (trans reducer)


{-| Emit a value mapping the reply.
-}
emit :
    Transducer afterInput afterOutput thisInput thisOutput
    -> (afterOutput -> thisOutput)
    -> afterInput
    -> Reducer afterInput afterOutput
    -> Reply thisInput thisOutput
emit trans outputMap x reducer =
    case reduce x reducer of
        Halt x ->
            Halt (outputMap x)

        Continue newReducer ->
            Continue (trans newReducer)


{-| Applies a `Continue` reducer to the input if possible.
-}
apply : input -> Reply input output -> Reply input output
apply x reply =
    case reply of
        Continue reducer ->
            reduce x reducer

        Halt _ ->
            reply


{-| Make a transducer.
-}
transducer : (thisInput -> Reducer afterInput afterOutput -> Reply thisInput thisOutput) -> (Reducer afterInput afterOutput -> thisOutput) -> Transducer afterInput afterOutput thisInput thisOutput
transducer mapReduce mapFinish reducer =
    Reducer
        (flip mapReduce reducer)
        (\() -> mapFinish reducer)


{-| Make a simple transducer which doesn't do anything fancy on finish.
-}
simpleTransducer : (thisInput -> Reducer afterInput output -> Reply thisInput output) -> Transducer afterInput output thisInput output
simpleTransducer f ((Reducer _ finishF) as reducer) =
    Reducer
        (flip f reducer)
        finishF


{-| Given a function to apply the elements of a collection to a `Reducer`, applies the elements of each collection ingested to the `Reducer`.
-}
concat : (Reducer input output -> collection -> Reply input output) -> Transducer input output collection output
concat stepper =
    simpleTransducer
        (\xs reducer ->
            mapReply (concat stepper) identity (stepper reducer xs)
        )


{-| Maps inputs.
-}
mapInput : (thisInput -> afterInput) -> Transducer afterInput output thisInput output
mapInput f =
    simpleTransducer
        (\x reducer ->
            emit (mapInput f) identity (f x) reducer
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
            Continue (fold step (step x state) reducer)
        )
        (\reducer -> finishWith state reducer)


{-| Upon ingesting an input, just keeps emitting that input until a `Halt` reply is received.

This `Transducer` is potentially infinite, so make sure that whatever is passed will eventually `Halt` on its own.

-}
repeatedly : Transducer afterInput output afterInput output
repeatedly =
    simpleTransducer
        (\input reducer ->
            case reduce input reducer of
                Halt x ->
                    Halt x

                Continue newReduction ->
                    reduce input (repeatedly newReduction)
        )


{-| If `n <= 0`, then `Halt` without emitting. Otherwise, pass through until `n` elements have been emitted.
-}
take : Int -> Transducer input output input output
take n =
    simpleTransducer
        (\x reducer ->
            if n <= 0 then
                Halt (finish reducer)
            else if n == 1 then
                finishWith x reducer |> Halt
            else
                emit (take (n - 1)) identity x reducer
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
            Continue (reverseHelper (x :: state) reducer)
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
                emit (filter predicate) identity x reducer
            else
                Continue (filter predicate reducer)
        )


{-| Ignore the first n elements and then emit everything else.
-}
drop : Int -> Transducer input output input output
drop n =
    simpleTransducer
        (\x reducer ->
            if n <= 0 then
                emit identity identity x reducer
            else
                Continue (drop (n - 1) reducer)
        )


{-| Emits the padding value before each value after the first.
-}
intersperse : input -> Transducer input output input output
intersperse padding =
    simpleTransducer
        (\x reducer ->
            emit
                (mapInput (\x -> [ padding, x ]) |> compose (concat listStepper))
                identity
                x
                reducer
        )


listStepper : Reducer input output -> List input -> Reply input output
listStepper reducer xs =
    case xs of
        [] ->
            Continue reducer

        x :: rest ->
            case reduce x reducer of
                Halt output ->
                    Halt output

                Continue nextReducer ->
                    listStepper nextReducer rest


{-| Emits `False` and `Halt`s if it receives an element. Emits `True` on finish.
-}
isEmpty : Transducer Bool output input output
isEmpty =
    transducer
        (\x reducer ->
            finishWith False reducer |> Halt
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
            lengthHelper (count + 1) reducer |> Continue
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
                finishWith True reducer |> Halt
            else
                member comp reducer |> Continue
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
    partitionHelper predicate (Continue (trueReducer cap)) (Continue (falseReducer cap))


partitionHelper :
    (input -> Bool)
    -> Reply input trueOutput
    -> Reply input falseOutput
    -> Transducer ( trueOutput, falseOutput ) output input output
partitionHelper predicate trueReply falseReply =
    transducer
        (\x reducer ->
            if predicate x then
                Continue (partitionHelper predicate (apply x trueReply) falseReply reducer)
            else
                Continue (partitionHelper predicate trueReply (apply x falseReply) reducer)
        )
        (\reducer ->
            finishWith ( finishReply trueReply, finishReply falseReply ) reducer
        )


{-| Upon ingesting the tuple, emits the value n times.
-}
repeat : Transducer input output ( Int, input ) output
repeat =
    simpleTransducer
        (\( n, x ) reducer ->
            doRepeat n x (Continue reducer)
                |> mapReply repeat identity
        )


doRepeat : Int -> input -> Reply input output -> Reply input output
doRepeat n x reply =
    if n <= 0 then
        reply
    else
        case reply of
            Halt _ ->
                reply

            Continue reducer ->
                doRepeat (n - 1) x (reduce x reducer)


{-| Map the transducer output value.
-}
mapOutput : (afterOutput -> thisOutput) -> Transducer input afterOutput input thisOutput
mapOutput f =
    transducer
        (\x reducer ->
            emit (mapOutput f) f x reducer
        )
        (f << finish)


{-| Provide the default value if output is `Nothing`.
-}
withDefault : output -> Transducer input (Maybe output) input output
withDefault value =
    mapOutput (Maybe.withDefault value)
