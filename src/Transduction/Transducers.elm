module Transduction.Transducers
    exposing
        ( elementsOf
        , toList
        , fold
        , unfold
        , map
        , take
        , drop
        , filter
        , reverse
        , count
        )

{-| Actual `Transducer` implementations.

@docs elementsOf, fold, unfold, map, toList, take, drop, filter, reverse, count

-}

-- , last
-- , repeatedly
-- , intersperse
-- , isEmpty
-- , member
-- , partition
-- , repeat
-- , withDefault
-- , zipElements
-- , compareBy
-- , combinations

import Transduction
    exposing
        ( Transducer
        , terminate
        , waitForInput
        , produce
        , compose
        , push
        )


{-| Produces the values of each list it receives in order.
-}
elementsOf : Transducer (List a) a
elementsOf =
    unfold
        (\input ->
            case input of
                [] ->
                    Nothing

                x :: xs ->
                    Just ( x, xs )
        )


{-| Collects everything into a list which is produced on termination.
-}
toList : Transducer a (List a)
toList =
    fold (::) []
        |> compose (map List.reverse)


{-| Accumulates values until it receives termination signal at which time it produces the output then terminates.
-}
fold : (a -> b -> b) -> b -> Transducer a b
fold f acc =
    waitForInput
        (\() -> produce acc (\() -> terminate))
        (\input -> fold f (f input acc))


{-| Deconstructs inputs into elements and produces them until none remain.
-}
unfold : (b -> Maybe ( a, b )) -> Transducer b a
unfold f =
    waitForInput
        (\() -> terminate)
        (\input ->
            case f input of
                Nothing ->
                    unfold f

                Just ( x, acc ) ->
                    produce x (\() -> unfold f |> push acc)
        )


{-| Maps inputs and immediately produces them.
-}
map : (a -> b) -> Transducer a b
map f =
    waitForInput
        (\() -> terminate)
        (\input -> produce (f input) (\() -> map f))


{-| Produces the first n elements received and then terminates.
-}
take : Int -> Transducer a a
take n =
    if n > 0 then
        waitForInput
            (\() -> terminate)
            (\input -> produce input (\() -> take (n - 1)))
    else
        terminate


{-| Ignores the first n elements then produces each element as it is received.
-}
drop : Int -> Transducer a a
drop n =
    if n > 0 then
        waitForInput
            (\() -> terminate)
            (\_ -> drop (n - 1))
    else
        map identity


{-| Only produces elements which pass the predicate.
-}
filter : (a -> Bool) -> Transducer a a
filter f =
    waitForInput
        (\() -> terminate)
        (\input ->
            if f input then
                produce input (\() -> filter f)
            else
                filter f
        )


{-| Accumulates elements and then produces them in last-in-first-out order on termination.
-}
reverse : Transducer a a
reverse =
    fold (::) []
        |> compose elementsOf


{-| Counts elements and produces the count on termination signal.
-}
count : Transducer a Int
count =
    fold (\_ n -> n + 1) 0



-- import Transduction.List.Shared as TList
-- import Lazy.List exposing (LazyList)
-- {-| Actually apply your transducer. Automatically composes `last` to the end.
-- -}
-- transduce : Transducer reducerInput (Maybe reducerInput) thisInput thisOutput -> thisInput -> thisOutput
-- transduce trans x =
--     finishWith x (compose last trans unit)
-- {-| Composes two transducers together. The parameter order is to make chaining using `|>` easier.
-- `first |> compose second |> compose third`
-- -}
-- compose :
--     Transducer reducerInput reducerOutput middleInput middleOutput
--     -> Transducer middleInput middleOutput thisInput thisOutput
--     -> Transducer reducerInput reducerOutput thisInput thisOutput
-- compose =
--     (>>)
-- {-| An example of a transducer which doesn't care about what reducer it receives. Effectively a `Reducer`, but not technically.
-- While it *could* chain with any transducer, since it can't actually interact, the type means it only works with transducers/reducers which don't do anything.
-- This transducer is automatically added to the end of a transducer chain when using `transduce`. This is to encourage you to make transducers which emit their final output instead of simply returning it. Combining this with `withDefault` will overcome the `Maybe`. I *think* that `last` is the only one of these pseudo-reducers you should actually need in normal practice.
-- -}
-- last : Transducer Never () input (Maybe input)
-- last =
--     lastHelper Nothing
-- lastHelper : Maybe input -> Transducer never1 never2 input (Maybe input)
-- lastHelper input =
--     forcedTransducer
--         (lastHelper << Just)
--         (\_ -> input)
-- {-| Given a function to apply the elements of a collection to a `Reducer`, applies the elements of each collection ingested to the `Reducer`.
-- -}
-- concat :
--     (collection -> Reducer input output -> Reducer input output)
--     -> Transducer input output collection output
-- concat stepper =
--     simpleTransducer
--         (\xs reducer ->
--             concat stepper (stepper xs reducer)
--         )
-- provideListElements : Transducer (List a) a
-- provideListElements =
-- noOutput (() -> )
-- {-| Maps inputs.
-- -}
-- mapInput : (thisInput -> reducerInput) -> Transducer reducerInput output thisInput output
-- mapInput f =
--     simpleTransducer
--         (\x reducer ->
--             mapInput f (reduce (f x) reducer)
--         )
-- {-| Each element updates the state which emits on finish.
-- -}
-- fold :
--     (input -> state -> state)
--     -> state
--     -> Transducer state output input output
-- fold step state =
--     transducer
--         (\x reducer ->
--             fold step (step x state) reducer
--         )
--         (\reducer -> finishWith state reducer)
-- {-| Upon ingesting an input, just keeps emitting that input until a `Halt` reply is received.
-- This `Transducer` is potentially infinite, so make sure that whatever is passed will eventually `Halt` on its own.
-- -}
-- repeatedly : Transducer input output input output
-- repeatedly =
--     simpleTransducer
--         (\input reducer ->
--             if isHalted reducer then
--                 reducer
--             else
--                 reduce input (repeatedly (reduce input reducer))
--         )
-- {-| If `n <= 0`, then `Halt` without emitting. Otherwise, pass through until `n` elements have been emitted.
-- -}
-- take : Int -> Transducer input output input output
-- take n =
--     advancedTransducer
--         (if n <= 0 then
--             Nothing
--          else
--             Just
--                 (\reducer ->
--                     if isHalted reducer then
--                         Nothing
--                     else if n == 1 then
--                         Just (\x -> finishWith x reducer |> halt)
--                     else
--                         Just (\x -> take (n - 1) (reduce x reducer))
--                 )
--         )
--         (\reducer -> finish reducer)
-- {-| On finish, emits a list of elements received in reverse order.
-- -}
-- reverse : Transducer (List input) output input output
-- reverse =
--     reverseHelper []
-- reverseHelper : List input -> Transducer (List input) output input output
-- reverseHelper state =
--     transducer
--         (\x reducer ->
--             reverseHelper (x :: state) reducer
--         )
--         (\reducer ->
--             finishWith state reducer
--         )
-- {-| Emits values where predicate is true.
-- -}
-- filter : (input -> Bool) -> Transducer input output input output
-- filter predicate =
--     simpleTransducer
--         (\x reducer ->
--             if predicate x then
--                 filter predicate (reduce x reducer)
--             else
--                 filter predicate reducer
--         )
-- {-| Ignore the first n elements and then emit everything else.
-- -}
-- drop : Int -> Transducer input output input output
-- drop n =
--     simpleTransducer
--         (\x reducer ->
--             if n <= 0 then
--                 reduce x reducer
--             else
--                 drop (n - 1) reducer
--         )
-- {-| Emits the padding value before each value after the first.
-- -}
-- intersperse : input -> Transducer input output input output
-- intersperse padding =
--     simpleTransducer
--         (\x reducer ->
--             (mapInput (\x -> [ padding, x ])
--                 |> compose (concat TList.emitter)
--             )
--                 (reduce x reducer)
--         )
-- {-| Emits `False` and `Halt`s if it receives an element. Emits `True` on finish.
-- -}
-- isEmpty : Transducer Bool output input output
-- isEmpty =
--     transducer
--         (\x reducer ->
--             finishWith False reducer |> halt
--         )
--         (\reducer ->
--             finishWith True reducer
--         )
-- {-| Emits the number of elements seen on finish.
-- -}
-- length : Transducer Int output input output
-- length =
--     lengthHelper 0
-- {-| Emits a count of elements seen on finish.
-- -}
-- lengthHelper : Int -> Transducer Int output input output
-- lengthHelper count =
--     transducer
--         (\x reducer ->
--             lengthHelper (count + 1) reducer
--         )
--         (\reducer ->
--             finishWith count reducer
--         )
-- {-| Emits a `Bool` indicating whether the value has been seen on finish.
-- -}
-- member : input -> Transducer Bool output input output
-- member comp =
--     transducer
--         (\x reducer ->
--             if x == comp then
--                 finishWith True reducer |> halt
--             else
--                 member comp reducer
--         )
--         (\reducer -> finishWith False reducer)
-- {-| "Applies one of two different reducers depending on the predicate. Emits a tuple of the reducers output on finish.."
-- -}
-- partition :
--     (input -> Bool)
--     -> Transducer trueInput (Maybe trueInput) input trueOutput
--     -> Transducer falseInput (Maybe falseInput) input falseOutput
--     -> Transducer ( trueOutput, falseOutput ) output input output
-- partition predicate trueReducer falseReducer =
--     partitionHelper predicate (compose last trueReducer unit) (compose last falseReducer unit)
-- partitionHelper :
--     (input -> Bool)
--     -> Reducer input trueOutput
--     -> Reducer input falseOutput
--     -> Transducer ( trueOutput, falseOutput ) output input output
-- partitionHelper predicate trueReply falseReply =
--     transducer
--         (\x reducer ->
--             if predicate x then
--                 partitionHelper predicate (reduce x trueReply) falseReply reducer
--             else
--                 partitionHelper predicate trueReply (reduce x falseReply) reducer
--         )
--         (\reducer ->
--             finishWith ( finish trueReply, finish falseReply ) reducer
--         )
-- {-| Upon ingesting the tuple, emits the value n times.
-- -}
-- repeat : Transducer input output ( Int, input ) output
-- repeat =
--     simpleTransducer
--         (\( n, x ) reducer ->
--             repeat (doRepeat n x reducer)
--         )
-- doRepeat : Int -> input -> Reducer input output -> Reducer input output
-- doRepeat n x reducer =
--     if n <= 0 || isHalted reducer then
--         reducer
--     else
--         doRepeat (n - 1) x (reduce x reducer)
-- {-| Map the transducer output value.
-- -}
-- mapOutput : (reducerOutput -> thisOutput) -> Transducer input reducerOutput input thisOutput
-- mapOutput f =
--     transducer
--         (\x reducer ->
--             mapOutput f (reduce x reducer)
--         )
--         (f << finish)
-- {-| Provide the default value if output is `Nothing`.
-- -}
-- withDefault : output -> Transducer input (Maybe output) input output
-- withDefault value =
--     mapOutput (Maybe.withDefault value)
-- {-| Takes several collections and sends the head of each one before sending the second element. If the first argument is `True`, then it will stop when a collection is empty, but if it is `False` it will just skip empty collections.
-- -}
-- zipElements :
--     Bool
--     -> (thisInput -> Maybe ( reducerInput, thisInput ))
--     -> Transducer reducerInput output thisInput output
-- zipElements =
--     zipElementsHelper []
-- zipElementsHelper :
--     List thisInput
--     -> Bool
--     -> (thisInput -> Maybe ( reducerInput, thisInput ))
--     -> Transducer reducerInput output thisInput output
-- zipElementsHelper collections haltOnEmpty elementF =
--     transducer
--         (\x reducer ->
--             case elementF x of
--                 Nothing ->
--                     if haltOnEmpty then
--                         finish reducer |> halt
--                     else
--                         zipElementsHelper collections haltOnEmpty elementF reducer
--                 Just ( n, rest ) ->
--                     zipElementsHelper (rest :: collections) haltOnEmpty elementF (reduce n reducer)
--         )
--         (zipElementsFinisher collections [] haltOnEmpty elementF)
-- zipElementsFinisher :
--     List collection
--     -> List collection
--     -> Bool
--     -> (collection -> Maybe ( element, collection ))
--     -> Reducer element output
--     -> output
-- zipElementsFinisher nextCollections currentCollections haltOnEmpty elementF reducer =
--     if isHalted reducer then
--         finish reducer
--     else
--         case currentCollections of
--             [] ->
--                 if List.isEmpty nextCollections then
--                     finish reducer
--                 else
--                     zipElementsFinisher [] (List.reverse nextCollections) haltOnEmpty elementF reducer
--             coll :: rest ->
--                 case elementF coll of
--                     Nothing ->
--                         if haltOnEmpty then
--                             finish reducer
--                         else
--                             zipElementsFinisher nextCollections rest haltOnEmpty elementF reducer
--                     Just ( x, next ) ->
--                         zipElementsFinisher
--                             (next :: nextCollections)
--                             rest
--                             haltOnEmpty
--                             elementF
--                             (reduce x reducer)
-- {-| Uses the function to find the element which beats them all. Emits `Nothing` if no items ingested. Emits `Just x` if only one item is ingested. Otherwise replaces this item if the function returns true (new elements are the first argument to the function).
-- -}
-- compareBy : (input -> input -> Bool) -> Transducer (Maybe input) output input output
-- compareBy =
--     compareByHelper Nothing
-- compareByHelper : Maybe input -> (input -> input -> Bool) -> Transducer (Maybe input) output input output
-- compareByHelper current f =
--     transducer
--         (\x ->
--             compareByHelper
--                 (current
--                     |> Maybe.map
--                         (\c ->
--                             if f x c then
--                                 x
--                             else
--                                 c
--                         )
--                     |> Maybe.withDefault x
--                     |> Just
--                 )
--                 f
--         )
--         (finishWith current)
-- {-| Emits pairs of combinations for all values seen so far.
-- -}
-- combinations : Transducer ( input, input ) output input output
-- combinations reducer =
--     combinationsHelper [] (concat TList.lazyEmitter reducer)
-- combinationsHelper : List input -> Transducer (LazyList ( input, input )) output input output
-- combinationsHelper xs =
--     simpleTransducer
--         (\x reducer ->
--             xs
--                 |> Lazy.List.fromList
--                 |> Lazy.List.map (\y -> ( y, x ))
--                 |> flip reduce reducer
--                 |> combinationsHelper (xs ++ [ x ])
--         )
