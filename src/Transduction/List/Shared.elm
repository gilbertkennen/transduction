module Transduction.List.Shared exposing (stepper)

import Transduction as Trans exposing (Reducer)


{-| Reduce elements of a `List` in order.
-}
stepper : List input -> Reducer input output -> Reducer input output
stepper xs reducer =
    case xs of
        [] ->
            reducer

        x :: rest ->
            if Trans.isHalted reducer then
                reducer
            else
                stepper rest (Trans.reduce x reducer)
