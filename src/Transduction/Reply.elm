module Transduction.Reply
    exposing
        ( Reply
        , halt
        , continue
        , empty
        , map
        , map2
        , mapContinue
        , andThen
        , andThenContinue
        , isGo
        , isStop
        , toHalt
        , toEmpty
        , refill
        , state
        )

{-| Building your own advanced transducers or steppers usually requires manually handling init and step replies.


# Type

@docs Reply


# Basics

@docs continue, empty, halt


# Transformations

@docs map, map2, mapContinue, andThen, andThenContinue


# Miscellaneous

@docs isStop, isGo, toHalt, toEmpty, refill, state

-}


{-| -}
type Reply state
    = Continue state
    | Halt state
    | Empty state


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


{-| Indicate that the emitter is unable to continue for being empty.
-}
empty : a -> Reply a
empty =
    Empty


{-| Transform a contained state regardless of if it is `continue` or `halt`.
-}
map : (a -> b) -> Reply a -> Reply b
map f x =
    case x of
        Halt state ->
            Halt (f state)

        Continue state ->
            Continue (f state)

        Empty state ->
            Empty (f state)


{-| -}
map2 : (a -> b -> c) -> Reply a -> Reply b -> Reply c
map2 f x y =
    andThen (\x_ -> map (f x_) y) x


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

        Empty state ->
            toEmpty (f state)

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


{-| Check if it is currently halt or empty.
-}
isStop : Reply a -> Bool
isStop reply =
    not (isGo reply)


{-| Check if it is currently continue.
-}
isGo : Reply a -> Bool
isGo reply =
    case reply of
        Continue _ ->
            True

        _ ->
            False


{-| Sets reply to halt.
-}
toHalt : Reply a -> Reply a
toHalt reply =
    case reply of
        Continue state ->
            Halt state

        Empty state ->
            Halt state

        Halt _ ->
            reply


{-| Sets reply to empty unless it is already halt.
-}
toEmpty : Reply a -> Reply a
toEmpty reply =
    case reply of
        Continue state ->
            Empty state

        Empty _ ->
            reply

        Halt _ ->
            reply


{-| Switches reply from empty to continue.
-}
refill : Reply a -> Reply a
refill reply =
    case reply of
        Empty state ->
            Continue state

        _ ->
            reply


{-| Extracts the state regardless of `continue` or `halt`.
-}
state : Reply state -> state
state reply =
    case reply of
        Continue state ->
            state

        Empty state ->
            state

        Halt state ->
            state
