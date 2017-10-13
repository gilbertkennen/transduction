module Transduction.Collection.List exposing (reducer, stepper)

{-| Implementations for `List`
@docs reducer stepper
-}

import Transduction.Reply as Reply exposing (Reply)
import Transduction exposing (Reducer, Stepper)


{-| A `Reducer` which builds an ordered list of elements.
-}
reducer : Reducer (List a) a (List a)
reducer =
    Transduction.reducer
        (Reply.continue [])
        (\x state -> Reply.continue (x :: state))
        List.reverse


{-| A `Stepper` which sends elements into the step function until a `Halt` or empty list.
-}
stepper : Stepper state (List a) a
stepper f state xs =
    if Reply.isHalted state then
        state
    else
        case xs of
            [] ->
                state

            x :: rest ->
                stepper f (Reply.andThenContinue (f x) state) rest
