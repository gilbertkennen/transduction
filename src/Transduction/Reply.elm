module Transduction.Reply exposing (Reply(Continue, Halt), map, andThen, halt)

{-| Building your own advanced transducers or steppers usually requires manually handling init and step replies.

`Halt` is a stronger state than `Continue` so I haven't provided any easy way to switch from `Halt` to `Continue`.

-}


{-| Halt means that the transducer should not be run again with the state. Continue means that you *may* step again with the state, but might not (the collection might be empty).
-}
type Reply state
    = Continue state
    | Halt state


map : (a -> b) -> Reply a -> Reply b
map f x =
    case x of
        Halt state ->
            Halt (f state)

        Continue state ->
            Continue (f state)


{-| This ensures that Halt states remain Halt while giving the option of switching to Halt.
-}
andThen : (a -> Reply b) -> Reply a -> Reply b
andThen f x =
    case x of
        Halt state ->
            halt (f state)

        Continue state ->
            f state


{-| Sets reply to Halt.
-}
halt : Reply a -> Reply a
halt reply =
    case reply of
        Continue state ->
            Halt state

        _ ->
            reply
