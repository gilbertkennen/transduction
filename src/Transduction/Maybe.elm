module Transduction.Maybe exposing (stepper, maybe)

{-| Transducer helpers for `Maybe`.

@docs stepper, maybe

-}

import Transduction as Trans exposing (Reducer, Reply(Continue, Halt), Transducer)


{-| When you only want to emit when you have a value.
-}
stepper : Reducer input output -> Maybe input -> Reply input output
stepper reducer maybeX =
    case maybeX of
        Nothing ->
            Continue reducer

        Just x ->
            Trans.reduce reducer x


{-| Convenience application of `stepper`.
-}
maybe : Transducer input output (Maybe input) output
maybe =
    Trans.concat stepper
