module Transduction.List exposing (stepper, concat, transduce)

{-| A stepper function for use with `Transducer`s like `concat`.

@docs stepper, concat, transduce

-}

import Transduction as Trans
    exposing
        ( Reducer
        , Transducer
        )
import Transduction.List.Shared as TListS
import Transduction.Transducers as Transducers


{-| Reduce elements of a `List` in order.
-}
stepper : List input -> Reducer input output -> Reducer input output
stepper =
    TListS.stepper


{-| A special concat just for `List`s.
-}
concat : Transducer input output (List input) output
concat =
    Transducers.concat stepper


{-| Run the transducer against a `List` of inputs.
-}
transduce : Transducer afterInput (Maybe afterInput) thisInput thisOutput -> List thisInput -> thisOutput
transduce transducer xs =
    Transducers.transduce (concat |> Transducers.compose transducer) xs
