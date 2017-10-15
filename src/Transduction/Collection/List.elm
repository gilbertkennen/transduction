module Transduction.Collection.List exposing (reducer, emitter)

{-| Implementations for `List`
@docs reducer emitter
-}

import Transduction.Reply as Reply exposing (Reply)
import Transduction exposing (Reducer, Emitter, transducer)


{-| A `Reducer` which builds an ordered list of elements.
-}
reducer : Reducer (List a) a (List a)
reducer =
    Transduction.reducer
        (Reply.continue [])
        (\x state -> Reply.continue (x :: state))
        List.reverse


emitter :
    List afterInput
    -> Emitter afterState afterInput afterResult ( List afterInput, afterState ) afterResult
emitter xs =
    transducer
        (Reply.map ((,) xs))
        (\step () ( ys, state ) ->
            case ys of
                [] ->
                    Reply.empty ( [], state )

                y :: rest ->
                    step y state |> Reply.map ((,) rest)
        )
        ((>>) Tuple.second)



-- {-| A `Stepper` which sends elements into the step function until a `Halt` or empty list.
-- -}
-- stepper : Stepper state (List a) a
-- stepper f state xs =
--     if Reply.isHalted state then
--         state
--     else
--         case xs of
--             [] ->
--                 state
--             x :: rest ->
--                 stepper f (Reply.andThenContinue (f x) state) rest
