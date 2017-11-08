module Transduction.List.Shared exposing (stepper)

import Transduction as Trans exposing (Reducer)


{-| Reduce elements of a `List` in order.
-}
stepper : Reducer input output -> List input -> Reducer input output
stepper reducer xs =
    case xs of
        [] ->
            reducer

        x :: rest ->
            if Trans.isHalted reducer then
                reducer
            else
                stepper (Trans.reduce x reducer) rest
