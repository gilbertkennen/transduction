module Transduction.List exposing (stepper, concat, reduce)

{-| A stepper function for use with `Transducer`s like `concat`.
-}

import Transduction as Trans
    exposing
        ( Reducer
        , Reply(Continue, Halt)
        , Transducer
        , fold
        , compose
        , mapInput
        )


stepper : Reducer input output -> List input -> Reply input output
stepper reducer xs =
    case xs of
        [] ->
            Continue reducer

        x :: rest ->
            case Trans.reduce reducer x of
                Halt output ->
                    Halt output

                Continue nextReducer ->
                    stepper nextReducer rest


{-| A special concat just for `List`s.
-}
concat : Transducer input output (List input) output
concat =
    Trans.concat stepper


{-| Reduce given a transducer.
-}
reduce : Transducer afterInput (Maybe afterInput) thisInput thisOutput -> List thisInput -> thisOutput
reduce transducer xs =
    Trans.transduce (concat |> compose transducer) xs
