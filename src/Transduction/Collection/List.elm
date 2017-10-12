module Transduction.Collection.List exposing (reducer, stepper)

{-| Implementations for `List`
-}

import Transduction.Reply as Reply exposing (Reply)
import Transduction exposing (Reducer, Stepper)


reducer : Reducer (List a) (List a) a
reducer =
    Transduction.reducer
        (Reply.continue [])
        (\x state -> Reply.continue (x :: state))
        List.reverse


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
