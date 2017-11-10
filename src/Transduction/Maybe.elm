module Transduction.Maybe exposing (emitter, maybe)

{-| Transducer helpers for `Maybe`.

@docs emitter, maybe

-}

import Transduction as Trans
    exposing
        ( Reducer
        , Transducer
        )
import Transduction.Transducers as Transducers


{-| When you only want to emit when you have a value.
-}
emitter : Maybe input -> Reducer input output -> Reducer input output
emitter maybeX reducer =
    case maybeX of
        Nothing ->
            reducer

        Just x ->
            Trans.reduce x reducer


{-| Convenience application of `emitter`.
-}
maybe : Transducer input output (Maybe input) output
maybe =
    Transducers.concat emitter
