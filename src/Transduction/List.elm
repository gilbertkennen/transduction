module Transduction.List exposing (stepper, reducer, concat, reduce)

{-| A stepper function for use with `Transducer`s like `concat`.
-}

import Transduction
    exposing
        ( Reducer
        , Reply(Continue, Halt)
        , Transducer
        , fold
        , compose
        , mapInput
        , cap
        , finish
        )


stepper : Reducer input output -> List input -> Reply input output
stepper reducer xs =
    case xs of
        [] ->
            Continue reducer

        x :: rest ->
            case reducer (Just x) of
                Halt output ->
                    Halt output

                Continue nextReducer ->
                    stepper nextReducer rest


{-| When you want the output to be a `List input`.
-}
reducer : Reducer input (List input)
reducer =
    fold (::) [] |> compose (mapInput List.reverse) |> (|>) cap


{-| A special concat just for `List`s.
-}
concat : Transducer input output (List input) output
concat =
    Transduction.concat stepper


{-| Reduce given a transducer.
-}
reduce : Transducer afterInput afterInput thisInput thisOutput -> List thisInput -> thisOutput
reduce transducer xs =
    (concat |> compose transducer) cap (Just xs) |> finish
