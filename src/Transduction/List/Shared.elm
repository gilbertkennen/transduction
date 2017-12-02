module Transduction.List.Shared exposing (emitter, lazyEmitter)

import Transduction as Trans exposing (Transducer)
import Lazy.List exposing (LazyList)


{-| Reduce elements of a `List` in order.
-}
emitter : List input -> Transducer input output input output
emitter xs reducer =
    case xs of
        [] ->
            reducer

        x :: rest ->
            if Trans.isHalted reducer then
                reducer
            else
                emitter rest (Trans.reduce x reducer)


lazyEmitter : LazyList input -> Transducer input output input output
lazyEmitter xs reducer =
    if Trans.isHalted reducer then
        reducer
    else
        case Lazy.List.headAndTail xs of
            Nothing ->
                reducer

            Just ( x, rest ) ->
                lazyEmitter rest (Trans.reduce x reducer)
