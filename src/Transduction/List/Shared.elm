module Transduction.List.Shared exposing (emitter)

import Transduction as Trans exposing (Reducer)


{-| Reduce elements of a `List` in order.
-}
emitter : List input -> Reducer input output -> Reducer input output
emitter xs reducer =
    case xs of
        [] ->
            reducer

        x :: rest ->
            if Trans.isHalted reducer then
                reducer
            else
                emitter rest (Trans.reduce x reducer)
