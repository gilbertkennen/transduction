module Transduction.List exposing (emitter, concat, transduce)

{-| A emitter function for use with `Transducer`s like `concat`.

@docs emitter, concat, transduce

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
emitter : List input -> Reducer input output -> Reducer input output
emitter =
    TListS.emitter


{-| A special concat just for `List`s.
-}
concat : Transducer input output (List input) output
concat =
    Transducers.concat emitter


{-| Run the transducer against a `List` of inputs.
-}
transduce : Transducer afterInput (Maybe afterInput) thisInput thisOutput -> List thisInput -> thisOutput
transduce transducer xs =
    Transducers.transduce (concat |> Transducers.compose transducer) xs
