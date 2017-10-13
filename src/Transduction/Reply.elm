module Transduction.Reply
    exposing
        ( Reply
        , halt
        , continue
        , map
        , mapContinue
        , andThen
        , andThenContinue
        , isHalted
        , toHalt
        , state
        )

{-| Building your own advanced transducers or steppers usually requires manually handling init and step replies.

`Halt` is a stronger state than `Continue` so I haven't provided any easy way to switch from `Halt` to `Continue`.


# Type

@docs Reply


# Basics

@docs continue, halt


# Transformations

@docs map, mapContinue, andThen, andThenContinue


# Miscellaneous

@docs isHalted, toHalt, state

-}


{-| Halt means that the transducer should not be run again with the state. Continue means that you *may* step again with the state, but might not (the collection might be empty).
-}
type Reply state
    = Continue state
    | Halt state


{-| Indicate that continuing to the next element is acceptable.
-}
continue : a -> Reply a
continue =
    Continue


{-| Indicate that no new elements should be sent.
-}
halt : a -> Reply a
halt =
    Halt


{-| Transform a contained state regardless of if it is `continue` or `halt`.
-}
map : (a -> b) -> Reply a -> Reply b
map f x =
    case x of
        Halt state ->
            Halt (f state)

        Continue state ->
            Continue (f state)


{-| Transform a contained state only if it is `continue`.
-}
mapContinue : (a -> a) -> Reply a -> Reply a
mapContinue f reply =
    case reply of
        Continue state ->
            Continue (f state)

        _ ->
            reply


{-| This ensures that Halt states remain Halt while giving the option of switching to Halt.
-}
andThen : (a -> Reply b) -> Reply a -> Reply b
andThen f x =
    case x of
        Halt state ->
            toHalt (f state)

        Continue state ->
            f state


{-| Only applies if it is `continue`.
-}
andThenContinue : (a -> Reply a) -> Reply a -> Reply a
andThenContinue f reply =
    case reply of
        Continue state ->
            f state

        _ ->
            reply


{-| Check if it is currently `halt`.
-}
isHalted : Reply a -> Bool
isHalted reply =
    case reply of
        Continue _ ->
            False

        Halt _ ->
            True


{-| Sets reply to Halt.
-}
toHalt : Reply a -> Reply a
toHalt reply =
    case reply of
        Continue state ->
            Halt state

        _ ->
            reply


{-| Extracts the state regardless of `continue` or `halt`.
-}
state : Reply state -> state
state reply =
    case reply of
        Continue state ->
            state

        Halt state ->
            state
