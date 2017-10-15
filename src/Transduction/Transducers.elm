module Transduction.Transducers
    exposing
        ( map
        , statefulMap
        , take
        , drop
        , withIndex
        , withCount
        , concat
        , reverse
        , filter
        , intersperse
        , isEmpty
        , length
        , member
        )

{-|


# Transducers

@docs map, statefulMap, take, drop, withIndex, withCount, concat, reverse


# Reducers

@docs length

-}

import Transduction exposing (Transducer(Transducer), Reducer, Stepper, transducer, reducer)
import Transduction.Reply as Reply exposing (Reply)
import Transduction.Collection.List as TCList


{-| Maps the collection's element values, not the result.
-}
map : (thisInput -> afterInput) -> Transducer state afterInput result state thisInput result
map f =
    transducer identity ((>>) f) identity


{-| Sometimes you need to remember a bit of state while mapping.
-}
statefulMap :
    thisState
    -> (thisInput -> thisState -> ( afterInput, thisState ))
    -> Transducer afterState afterInput result ( thisState, afterState ) thisInput result
statefulMap init1 step1 =
    transducer
        (\init2 -> Reply.map ((,) init1) init2)
        (\step2 ->
            (\x ( state1, state2 ) ->
                step1 x state1
                    |> (\( newX, newState1 ) ->
                            Reply.map ((,) newState1) (step2 newX state2)
                       )
            )
        )
        ((>>) Tuple.second)


{-| Include an index with each element.
-}
withIndex : Transducer afterState ( Int, thisInput ) result ( Int, afterState ) thisInput result
withIndex =
    statefulMap 0 (\x n -> ( ( n, x ), n + 1 ))


{-| Stop the iteration after the specified number of elements.
-}
take : Int -> Transducer afterState input result ( Int, afterState ) input result
take n =
    transducer
        (\init ->
            Reply.andThen
                (\state ->
                    if n <= 0 then
                        Reply.halt ( n, state )
                    else
                        Reply.continue ( n, state )
                )
                init
        )
        (\step x ( m, state ) ->
            Reply.andThen
                (\newState ->
                    if m <= 1 then
                        Reply.halt ( m - 1, newState )
                    else
                        Reply.continue ( m - 1, newState )
                )
                (step x state)
        )
        ((>>) Tuple.second)


{-| Skip the first n elements. Negatives count as 0.
-}
drop : Int -> Transducer afterState input result ( Int, afterState ) input result
drop n =
    transducer
        (Reply.map ((,) n))
        (\step x ( m, state ) ->
            if m <= 0 then
                Reply.map ((,) m) (step x state)
            else
                Reply.continue ( m - 1, state )
        )
        ((>>) Tuple.second)


{-| Attach a count of elements to the final result.
-}
withCount : Transducer afterState input afterResult ( Int, afterState ) input ( Int, afterResult )
withCount =
    transducer
        (Reply.map ((,) 0))
        (\step x ( n, state ) -> Reply.map ((,) (n + 1)) (step x state))
        Tuple.mapSecond


{-| Given an appropriate `Stepper` function, deconstruct each collection passed in and pass elements down instead.
-}
concat :
    Stepper state collection afterInput
    -> Transducer state afterInput result state collection result
concat stepper =
    transducer
        identity
        (\step collection state ->
            stepper step (Reply.continue state) collection
        )
        identity


{-| Re-emits elements in reverse order. Only works on finite lists.
-}
reverse : Transducer afterState input result (List input) input result
reverse =
    Transducer
        (\( init, step, finish ) ->
            ( Reply.map (\_ -> []) init
            , \x cache -> Reply.continue (x :: cache)
            , \cache -> TCList.stepper step init cache |> Reply.state |> finish
            )
        )


{-| Keeps only elements which match the predicate.
-}
filter : (input -> Bool) -> Transducer state input result state input result
filter f =
    transducer
        identity
        (\step x state ->
            if f x then
                step x state
            else
                Reply.continue state
        )
        identity


{-| Emits the provided element between elements.
-}
intersperse : input -> Transducer state input result ( Bool, state ) input result
intersperse x =
    transducer
        (Reply.map ((,) True))
        (\step y ( firstRun, state ) ->
            if firstRun then
                step y state |> Reply.map ((,) False)
            else
                TCList.stepper step (Reply.continue state) [ x, y ] |> Reply.map ((,) False)
        )
        ((>>) Tuple.second)


{-| Halts with `False` if any elements are received, otherwise is `True`.
-}
isEmpty : Reducer Bool input Bool
isEmpty =
    reducer
        (Reply.continue True)
        (\x _ -> Reply.halt False)
        identity


{-| Returns the number of elements passed to it.
-}
length : Reducer Int input Int
length =
    reducer
        (Reply.continue 0)
        (\_ acc -> Reply.continue (acc + 1))
        identity


{-| Halts with `True` if the element is passed in. Otherwise is `False`.
-}
member : input -> Reducer Bool input Bool
member x =
    reducer
        (Reply.continue False)
        (\y _ ->
            if x == y then
                Reply.halt True
            else
                Reply.continue False
        )
        identity
