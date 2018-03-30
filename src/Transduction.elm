module Transduction
    exposing
        ( Transducer
        , TransducerStatus(..)
        , compose
        , terminate
        , waitForInput
        , produce
        , terminateSignal
        , push
        , pop
        , status
        )

{-| Transducers are composable structures which process elements one at a time.


# Types

@docs Transducer


# Basic Transducers

@docs waitForInput, produce, terminate


# Composition

@docs compose


# Doing Work

@docs push, pop, TransducerStatus, status, terminateSignal

-}

import Queue exposing (Queue)
import Lazy exposing (Lazy, lazy, force)


{-| A transducer takes inputs from a wire and sends outputs on a wire, can terminate, and can take an end-of-computation signal.
-}
type Transducer input output
    = Use (() -> Transducer input output) (input -> Transducer input output)
    | Produce (Queue input) output (Lazy (Transducer input output))
    | Term
    | Delay (Queue input) (Lazy (Transducer input output))


{-| The current status of a transducer. `Producing` gives you the value on the output wire and the next version of the transducer.
-}
type TransducerStatus input output
    = Terminated
    | WaitingForInput
    | Producing output (Transducer input output)


{-| Two transducers with a common interface can be composed into a single transducer.

    Takes inputs in seemingly backward order in order to facilitate `first |> compose second` style pipelines.

-}
compose : Transducer a output -> Transducer input a -> Transducer input output
compose transR transL =
    case transR of
        Term ->
            -- Term on the right, ignore the left. Term base case.
            Term

        Produce queueR outputR lazyContR ->
            -- Processing waits until the output wire is clear. Produce base case.
            Produce
                Queue.empty
                outputR
                (Lazy.map
                    (\transFRNew ->
                        -- Process the right-hand queue before looking left.
                        transL |> compose (processQueue queueR transFRNew)
                    )
                    lazyContR
                )

        Delay queueR lazyContR ->
            -- The work hasn't been done yet, nothing to see here.
            transL |> compose (continueWith queueR lazyContR)

        Use transTermFR transUseFR ->
            case transL of
                Use transTermFL transUseFL ->
                    -- If both
                    Use
                        -- If both are waiting, then the whole thing is waiting. Use base case.
                        (\() -> transTermFL () |> compose transR)
                        (\input -> transUseFL input |> compose transR)

                Produce queueL outputL lazyContL ->
                    -- The interface wire has a product, send it to the right transducer and see what happens.
                    Delay queueL lazyContL |> compose (transUseFR outputL)

                Term ->
                    -- Term signals propagate to the right. Once the right stops producing, we force Term.
                    Term
                        |> compose
                            (case transTermFR () of
                                Use _ _ ->
                                    -- Waiting for input is not allowed after receiving term signal.
                                    Term

                                transRNew ->
                                    transRNew
                            )

                Delay queueL lazyContL ->
                    -- The right transducer is waiting for the left transducer to produce something. Better figure out if it has anything to produce.
                    continueWith queueL lazyContL |> compose transR


{-| Push an input into the transducer. `Nothing` represents a termination signal. All pushing is lazy, so the work won't be done until you check the transducer's status.
-}
push : input -> Transducer input output -> Transducer input output
push input trans =
    case trans of
        Term ->
            Term

        Use transTermF transUseF ->
            -- We don't *actually* need to do this work until something is asking about it, so let's be lazy.
            Delay Queue.empty (lazy (\() -> transUseF input))

        Produce queue output lazyCont ->
            -- We are waiting for the out wire to be cleared. Queue any additional work.
            Produce (Queue.push input queue) output lazyCont

        Delay queue lazyCont ->
            -- Nobody seems to care if we have anything going right now, so might as well wait until that happens.
            Delay (Queue.push input queue) lazyCont


{-| When you just want to try to get the current output of the transducer.
-}
pop : Transducer input output -> Maybe output
pop trans =
    case trans of
        Produce _ output _ ->
            Just output

        _ ->
            Nothing


{-| Query the status of a transducer, getting the value on the output wire if present.
-}
status : Transducer input output -> TransducerStatus input output
status trans =
    case trans of
        Term ->
            Terminated

        Use _ _ ->
            WaitingForInput

        Produce queue output lazyCont ->
            Producing output (Delay queue lazyCont)

        Delay queue lazyCont ->
            -- Somebody finally wants to know what we are!
            status (continueWith queue lazyCont)


{-| An internal function which provides queued elements unless the queue is empty or the output wire has an element on it.
-}
processQueue : Queue input -> Transducer input output -> Transducer input output
processQueue queue trans =
    case trans of
        Produce otherQueue output lazyCont ->
            Produce (Queue.append queue otherQueue) output lazyCont

        Term ->
            trans

        Use transTermF transUseF ->
            case Queue.pop queue of
                Nothing ->
                    trans

                Just ( input, newQueue ) ->
                    processQueue newQueue (transUseF input)

        Delay queue lazyCont ->
            processQueue queue (continueWith queue lazyCont)


{-| An internal function which processes the queue against a lazy transducer.
-}
continueWith :
    Queue input
    -> Lazy (Transducer input output)
    -> Transducer input output
continueWith queue lazyCont =
    processQueue queue (force lazyCont)


{-| Send terminate signal and halt.
-}
terminate : Transducer input output
terminate =
    Term


{-| Don't send anything down the wire, but you can still set a new transducer state. This is the normal initial state of a transducer and should be used for any custom transducers you might create.

The first function is executed on term signal and the second function is executed on receiving input.

-}
waitForInput :
    (() -> Transducer input output)
    -> (input -> Transducer input output)
    -> Transducer input output
waitForInput =
    Use


{-| Send something down the wire and then set a new transducer state.
-}
produce : output -> (() -> Transducer input output) -> Transducer input output
produce output lazyTrans =
    -- I can't think of any good reason to encourage making transducers with existing queues.
    Produce Queue.empty output (lazy lazyTrans)


{-| Sends a termination signal that waits for the output wire to be empty before completing. You get the exact same effect if you `compose terminate myTransducer`.
-}
terminateSignal : Transducer input output -> Transducer input output
terminateSignal trans =
    Term |> compose trans
