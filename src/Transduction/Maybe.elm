module Transduction.Maybe exposing (stepper, maybe)

{-| Transducer helpers for `Maybe`.

@docs stepper, maybe

-}

import Transduction as Trans
    exposing
        ( Reducer
        , Transducer
        )
import Transduction.Transducers as Transducers


{-| When you only want to emit when you have a value.
-}
stepper : Maybe input -> Reducer input output -> Reducer input output
stepper maybeX reducer =
    case maybeX of
        Nothing ->
            reducer

        Just x ->
            Trans.reduce x reducer


{-| Convenience application of `stepper`.
-}
maybe : Transducer input output (Maybe input) output
maybe =
    Transducers.concat stepper
