module Transduction.Collection.List exposing (reducer, stepper)

{-| Implementations for `List`
-}

import Transduction.Reply exposing (Reply(Halt, Continue))
import Transduction exposing (Reducer(Reducer), Stepper)


reducer : Reducer (List a) (List a) a
reducer =
    Reducer
        (Continue [])
        (\x state -> Continue (x :: state))
        List.reverse


stepper : Stepper state (List a) a
stepper f state xs =
    case xs of
        [] ->
            state

        x :: rest ->
            case f x state of
                Halt newState ->
                    newState

                Continue newState ->
                    stepList f newState rest
