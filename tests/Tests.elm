module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import List.Extra
import Transduction as Trans
import Transduction.Transducers as TTrans


transductionSuite : Test
transductionSuite =
    describe "Fundamentals"
        [ describe "terminate"
            [ test "should begin as Terminated"
                (\() ->
                    Trans.terminate
                        |> shouldBeTerminated
                )
            ]
        , describe "waitForInput"
            [ test "should begin as WaitingForInput" <|
                \() ->
                    Trans.waitForInput
                        (\() -> Trans.terminate)
                        (\() -> Trans.terminate)
                        |> shouldBeWaitingForInput
            , test "should change state on using an element" <|
                \() ->
                    Trans.waitForInput
                        (\() -> Trans.terminate)
                        (\() -> Trans.terminate)
                        |> Trans.push ()
                        |> shouldBeTerminated
            ]
        , describe "produce"
            [ test "should begin as Output." <|
                \() ->
                    Trans.produce () (\() -> Trans.terminate)
                        |> shouldBeProducing ()
            , test "should not change on using an element" <|
                \() ->
                    Trans.produce () (\() -> Trans.terminate)
                        |> Trans.push ()
                        |> shouldBeProducing ()
            ]
        , describe "push"
            [ test "multiple pushed items should be processed first-in first-out" <|
                \() ->
                    identityTransducer
                        |> Trans.push False
                        |> Trans.push True
                        |> shouldContinueWith (shouldBeProducing True)
            , test "pushing to a producing transducer which has received a termination signal does nothing" <|
                \() ->
                    identityTransducer
                        |> Trans.push ()
                        |> Trans.terminateSignal
                        |> Trans.push ()
                        |> shouldContinueWith shouldBeTerminated
            ]
        , describe "compose"
            [ test "`terminate terminate` should terminate" <|
                \() ->
                    Trans.terminate
                        |> Trans.compose Trans.terminate
                        |> shouldBeTerminated
            , test "`waitForInput terminate` should terminate" <|
                \() ->
                    waitingTransducer
                        |> Trans.compose Trans.terminate
                        |> shouldBeTerminated
            , test "`produce terminate` should terminate" <|
                \() ->
                    (producingTransducer ())
                        |> Trans.compose Trans.terminate
                        |> shouldBeTerminated
            , test "`terminate waitForInput` should terminate" <|
                \() ->
                    Trans.terminate
                        |> Trans.compose waitingTransducer
                        |> shouldBeTerminated
            , test "`terminate produce` should produce" <|
                \() ->
                    Trans.terminate
                        |> Trans.compose (producingTransducer ())
                        |> shouldBeProducing ()
            , test "`terminate (produce => terminate)` should produce then terminate" <|
                \() ->
                    Trans.terminate
                        |> Trans.compose (identityTransducer |> Trans.push ())
                        |> shouldContinueWith shouldBeTerminated
            , test "`terminate (produce => waitForInput)` should produce then terminate" <|
                \() ->
                    Trans.terminate
                        |> Trans.compose (identityTransducer |> Trans.push ())
                        |> shouldContinueWith shouldBeTerminated
            , test "`waitForInput waitForInput` should wait for input" <|
                \() ->
                    waitingTransducer
                        |> Trans.compose waitingTransducer
                        |> shouldBeWaitingForInput
            , test "`waitForInput produce` should produce" <|
                \() ->
                    waitingTransducer
                        |> Trans.compose (producingTransducer ())
                        |> shouldBeProducing ()
            , test "`produce produce` should produce" <|
                \() ->
                    (producingTransducer ())
                        |> Trans.compose (producingTransducer ())
                        |> shouldBeProducing ()
            , test "`produce (produce => waitForInput)` should produce then the right transducer should use the left product" <|
                \() ->
                    (producingTransducer True)
                        |> Trans.compose (identityTransducer |> Trans.push False)
                        |> shouldContinueWith (shouldBeProducing True)
            , test "a producer with a pending queue should finish processing the queue before looking left" <|
                \() ->
                    Trans.terminate
                        |> Trans.compose
                            (identityTransducer |> Trans.push False |> Trans.push True)
                        |> shouldContinueWith (shouldBeProducing True)
            , test "term actions on the left transducer should activate on term signal" <|
                \() ->
                    (produceOnTerm ())
                        |> Trans.compose identityTransducer
                        |> Trans.terminateSignal
                        |> shouldBeProducing ()
            ]
        ]


shouldContinueWith :
    (Trans.Transducer input output -> Expect.Expectation)
    -> Trans.Transducer input output
    -> Expect.Expectation
shouldContinueWith expectF trans =
    case Trans.status trans of
        Trans.Producing _ transNew ->
            expectF transNew

        status ->
            Expect.fail
                ("This transducer has status "
                    ++ toString status
                    ++ " instead of `Producing`."
                )


waitingTransducer : Trans.Transducer ignored never
waitingTransducer =
    Trans.waitForInput
        (\_ -> waitingTransducer)
        (\_ -> waitingTransducer)


producingTransducer : output -> Trans.Transducer ignored output
producingTransducer output =
    Trans.produce output
        (\() -> producingTransducer output)


identityTransducer : Trans.Transducer a a
identityTransducer =
    Trans.waitForInput
        (\() -> Trans.terminate)
        (\input -> Trans.produce input (\() -> identityTransducer))


produceOnTerm : output -> Trans.Transducer ignored output
produceOnTerm output =
    Trans.waitForInput
        (\() -> Trans.produce output (\() -> Trans.terminate))
        (\_ -> produceOnTerm output)


shouldBeTerminated : Trans.Transducer input output -> Expect.Expectation
shouldBeTerminated trans =
    trans |> Trans.status |> Expect.equal Trans.Terminated


shouldBeWaitingForInput : Trans.Transducer input output -> Expect.Expectation
shouldBeWaitingForInput trans =
    trans |> Trans.status |> Expect.equal Trans.WaitingForInput


shouldBeProducing :
    output
    -> Trans.Transducer input output
    -> Expect.Expectation
shouldBeProducing output trans =
    case Trans.status trans of
        Trans.Producing val _ ->
            if val == output then
                Expect.pass
            else
                Expect.fail
                    ("Expected "
                        ++ toString output
                        ++ "on the output wire, but found "
                        ++ toString val
                        ++ " instead."
                    )

        status ->
            Expect.fail
                ("Expecting status Output, but got "
                    ++ toString status
                    ++ " instead."
                )


transducerSuite : Test
transducerSuite =
    describe "Transducers"
        [ fuzz (list bool) "map" <|
            \xs ->
                TTrans.map not
                    |> transducerExpectation xs (List.map not xs)
        , fuzz (list int) "fold" <|
            \xs ->
                TTrans.fold (+) 0
                    |> transducerExpectation xs [ List.sum xs ]
        , fuzz (list bool) "unfold" <|
            \xs ->
                TTrans.unfold
                    (\input ->
                        case input of
                            [] ->
                                Nothing

                            x :: rest ->
                                Just ( x, rest )
                    )
                    |> transducerExpectation [ xs ] xs
        , fuzz (list bool) "toList" <|
            \xs ->
                TTrans.toList
                    |> transducerExpectation xs [ xs ]
        , fuzz (list (list bool)) "elementsOf" <|
            \xs ->
                TTrans.elementsOf
                    |> transducerExpectation xs (List.concat xs)
        , fuzz (tuple ( int, list bool )) "take" <|
            \( n, xs ) ->
                TTrans.take n
                    |> transducerExpectation xs (List.take n xs)
        , fuzz (tuple ( int, list bool )) "drop" <|
            \( n, xs ) ->
                TTrans.drop n
                    |> transducerExpectation xs (List.drop n xs)
        , fuzz (tuple ( int, list int )) "fliter" <|
            \( n, xs ) ->
                TTrans.filter ((>) n)
                    |> transducerExpectation xs (List.filter ((>) n) xs)
        , fuzz (list bool) "filter" <|
            \xs ->
                TTrans.reverse
                    |> transducerExpectation xs (List.reverse xs)
        , fuzz (list unit) "count" <|
            \xs ->
                TTrans.count
                    |> transducerExpectation xs [ List.length xs ]
        ]


transducerExpectation :
    List input
    -> List output
    -> Trans.Transducer input output
    -> Expect.Expectation
transducerExpectation inputs outputs trans =
    (TTrans.elementsOf |> Trans.compose trans |> Trans.compose (TTrans.toList))
        |> Trans.push inputs
        |> Trans.terminateSignal
        |> shouldBeProducing outputs



--     [ describe "mapInput transducer"
--         [ fuzz (list int) "should map the values of the collection" <|
--             \xs ->
--                 let
--                     f =
--                         (+) 1
--                 in
--                     TList.transduce
--                         (T.mapInput f
--                             |-> expect (List.map f xs)
--                         )
--                         xs
--         ]
--     , describe "take"
--         [ fuzz2 int (list int) "should take (at most) the first n elements" <|
--             \n xs ->
--                 TList.transduce
--                     (T.take n
--                         |-> expect (List.take n xs)
--                     )
--                     xs
--         ]
--     , describe "drop"
--         [ fuzz2 int (list int) "should skip the first n elements" <|
--             \n xs ->
--                 TList.transduce
--                     (T.drop n
--                         |-> expect (List.drop n xs)
--                     )
--                     xs
--         ]
--     , describe "concat"
--         [ fuzz (list (list int)) "should send elements in order, deconstructing one level of `List`" <|
--             \xs ->
--                 TList.transduce
--                     (T.concat TList.emitter
--                         |-> expect (List.concat xs)
--                     )
--                     xs
--         ]
--     , describe "reverse"
--         [ fuzz (list int) "should reverse the elements passed to it" <|
--             \xs ->
--                 TList.transduce
--                     (T.reverse
--                         |-> T.concat TList.emitter
--                         |-> expect (List.reverse xs)
--                     )
--                     xs
--         ]
--     , describe "filter"
--         [ fuzz (list int) "should filter out `False` values" <|
--             \xs ->
--                 let
--                     predicate =
--                         (\x -> x % 2 == 0)
--                 in
--                     TList.transduce
--                         (T.filter predicate
--                             |-> expect (List.filter predicate xs)
--                         )
--                         xs
--         ]
--     , describe "intersperse"
--         [ fuzz (list int) "should put an extra element between each other element." <|
--             \xs ->
--                 TList.transduce
--                     (T.intersperse 0
--                         |-> expect (List.intersperse 0 xs)
--                     )
--                     xs
--         ]
--     , describe "repeatedly"
--         [ fuzz int "should just keep emitting whatever it was given until it receives `Halt`" <|
--             \x ->
--                 let
--                     n =
--                         128
--                 in
--                     T.transduce
--                         (T.repeatedly
--                             |-> T.take n
--                             |-> expect (List.repeat n x)
--                         )
--                         x
--         ]
--     , describe "fold"
--         [ fuzz (list int) "should fold values until it finishes" <|
--             \xs ->
--                 let
--                     sum =
--                         List.sum xs
--                 in
--                     TList.transduce
--                         (T.fold (+) 0
--                             |-> expect ([ sum ])
--                         )
--                         xs
--         ]
--     , describe "isEmpty"
--         [ fuzz (list unit) "emits True on empty and False on non-empty" <|
--             \xs ->
--                 TList.transduce
--                     (T.isEmpty
--                         |-> expect [ List.isEmpty xs ]
--                     )
--                     xs
--         ]
--     , describe "length"
--         [ fuzz (list unit) "emits a count of elements on finish" <|
--             \xs ->
--                 TList.transduce
--                     (T.length
--                         |-> expect [ List.length xs ]
--                     )
--                     xs
--         ]
--     , describe "member" <|
--         [ fuzz2 int (list int) "emits `True` and `Halt`s if it receives the value and emits `False` on finish" <|
--             \x xs ->
--                 TList.transduce
--                     (T.member x
--                         |-> expect [ List.member x xs ]
--                     )
--                     xs
--         ]
--     , describe "partition" <|
--         [ fuzz (list int) "should sort and reduce items based on the predicate" <|
--             \xs ->
--                 let
--                     predicate =
--                         (\x -> x % 2 == 0)
--                 in
--                     TList.transduce
--                         (T.partition predicate
--                             (T.reverse |-> T.withDefault [])
--                             (T.length |-> T.withDefault 0)
--                             |-> expect
--                                     [ ( List.reverse (List.filter predicate xs)
--                                       , List.length (List.filter (not << predicate) xs)
--                                       )
--                                     ]
--                         )
--                         xs
--         ]
--     , describe "repeat"
--         [ fuzz (list (tuple ( intRange 0 128, int ))) "should emit the value n times" <|
--             \xs ->
--                 let
--                     repeats =
--                         List.concatMap (uncurry List.repeat) xs
--                 in
--                     TList.transduce (T.repeat |-> expect repeats) xs
--         ]
--     , describe "mapOutput"
--         [ fuzz (maybe int) "should apply the map function to the output" <|
--             \maybeX ->
--                 T.transduce
--                     (TMaybe.maybe |-> T.mapOutput (Maybe.map ((+) 1)))
--                     maybeX
--                     |> Expect.equal (Maybe.map ((+) 1) maybeX)
--         ]
--     , describe "withDefault"
--         [ fuzz (maybe int) "should fix `Nothing` output values" <|
--             \maybeX ->
--                 T.transduce
--                     (TMaybe.maybe |-> T.withDefault 0)
--                     maybeX
--                     |> Expect.equal (Maybe.withDefault 0 maybeX)
--         ]
--     , describe "zipElements"
--         [ fuzz (list (list unit)) "should emit an element for each in a list when set to not halt on empty" <|
--             (\listOfLists ->
--                 TList.transduce
--                     (T.zipElements False listEmitter
--                         |-> T.length
--                         |-> expect [ listOfLists |> List.map List.length |> List.sum ]
--                     )
--                     listOfLists
--             )
--         , test "should emit items in order" <|
--             \() ->
--                 TList.transduce
--                     (T.zipElements False listEmitter
--                         |-> expect [ 1, 4, 6, 7, 2, 5, 8, 3 ]
--                     )
--                     [ [ 1, 2, 3 ], [ 4, 5 ], [ 6 ], [ 7, 8 ] ]
--         , test "should not emit items after reaching an empty list when set to halt on empty" <|
--             \() ->
--                 TList.transduce
--                     (T.zipElements True listEmitter
--                         |-> expect [ 1, 4, 6, 7, 2, 5 ]
--                     )
--                     [ [ 1, 2, 3 ], [ 4, 5 ], [ 6 ], [ 7, 8 ] ]
--         , test "should even halt early if ingesting an empty collection" <|
--             \() ->
--                 TList.transduce
--                     (T.zipElements True listEmitter
--                         |-> expect [ 1, 4 ]
--                     )
--                     [ [ 1, 2, 3 ], [ 4, 5 ], [], [ 6 ], [ 7, 8 ] ]
--         ]
--     , describe "compareBy"
--         [ fuzz (list int) "should correctly get maximum using `>`" <|
--             \xs ->
--                 TList.transduce
--                     (T.compareBy (>) |-> expect [ List.maximum xs ])
--                     xs
--         ]
--     , describe "combinations"
--         [ test "should give each combination for each value presented" <|
--             \() ->
--                 TList.transduce
--                     (T.combinations
--                         |-> expect
--                                 [ ( 1, 2 )
--                                 , ( 1, 3 )
--                                 , ( 2, 3 )
--                                 , ( 1, 4 )
--                                 , ( 2, 4 )
--                                 , ( 3, 4 )
--                                 ]
--                     )
--                     [ 1, 2, 3, 4 ]
--         ]
--     ]
-- listEmitter : List a -> Maybe ( a, List a )
-- listEmitter xs =
--     Maybe.map2 (,) (List.head xs) (List.tail xs)
