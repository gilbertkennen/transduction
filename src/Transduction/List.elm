module Transduction.List exposing (stepper, concat, transduce)

{-| A stepper function for use with `Transducer`s like `concat`.

@docs stepper, concat, transduce

-}

import Transduction as Trans
    exposing
        ( Reducer
        , Transducer
        , compose
        )


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


{-| A special concat just for `List`s.
-}
concat : Transducer input output (List input) output
concat =
    Trans.concat stepper


{-| Run the transducer against a `List` of inputs.
-}
transduce : Transducer afterInput (Maybe afterInput) thisInput thisOutput -> List thisInput -> thisOutput
transduce transducer xs =
    Trans.transduce (concat |> compose transducer) xs
