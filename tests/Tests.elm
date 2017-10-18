module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Transduction as T
import Transduction.List as TList


(|->) :
    T.Transducer betweenInput betweenResult thisInput thisResult
    -> T.Transducer afterInput afterResult betweenInput betweenResult
    -> T.Transducer afterInput afterResult thisInput thisResult
(|->) =
    flip T.compose
infixr 8 |->


expect : List input -> T.Transducer Expect.Expectation output input output
expect xs reduction maybeX =
    let
        fail message =
            Just (Expect.fail message)
                |> reduction
                |> T.mapReply (expect xs) identity
    in
        case ( maybeX, xs ) of
            ( Nothing, [] ) ->
                (reduction (Just Expect.pass))
                    |> T.mapReply (expect []) identity

            ( Just x, [] ) ->
                fail ("Tried to consume " ++ toString x ++ " but list is empty.")

            ( Nothing, xs ) ->
                fail ("Did not consume: " ++ toString xs)

            ( Just x, y :: rest ) ->
                if x == y then
                    T.Continue (expect rest reduction)
                else
                    fail ("Was given " ++ toString x ++ " but expected " ++ toString y)



-- emitterSuite : Test
-- emitterSuite =
--     describe "Steppers"
--         [ describe "list stepper"
--             [ fuzz (list int) "should emit elements in order" <|
--                 \xs ->
--                     T.reduce (TList.emitter xs |-> expect xs)
--             , fuzz2 int (list int) "should stop early when halted" <|
--                 \n xs ->
--                     T.reduce
--                         (TList.emitter xs |-> Trans.take n |-> expect (List.take n xs))
--             ]
--         ]
-- reducerSuite : Test
-- reducerSuite =
--     describe "Reducers"
--         [ describe "list reducer"
--             [ fuzz (list int) "returns a list of elements in order" <|
--                 \xs ->
--                     xs
--                         |> listReduce TList.reducer
--                         |> Expect.equal xs
--             ]
--         ]


basicsSuite : Test
basicsSuite =
    describe "Basics"
        [ describe "cap"
            [ test "should `Continue` on `Nothing`" <|
                \() ->
                    case T.cap Nothing of
                        T.Continue _ ->
                            Expect.pass

                        T.Halt _ ->
                            Expect.fail "halted"
            , fuzz int "should `Halt x` on `Just x`" <|
                \x ->
                    Expect.equal (T.cap (Just x)) (T.Halt x)
            ]
        ]


transducerSuite : Test
transducerSuite =
    describe "Transducers"
        [ describe "mapInput transducer"
            [ fuzz (list int) "should map the values of the collection" <|
                \xs ->
                    let
                        f =
                            (+) 1
                    in
                        TList.reduce (T.mapInput f |-> expect (List.map f xs)) xs
            ]
        , describe "take"
            [ fuzz2 int (list int) "should take (at most) the first n elements" <|
                \n xs ->
                    TList.reduce (T.take n |-> expect (List.take n xs)) xs
            ]
        , describe "drop"
            [ fuzz2 int (list int) "should skip the first n elements" <|
                \n xs ->
                    TList.reduce (T.drop n |-> expect (List.drop n xs)) xs
            ]
        , describe "concat"
            [ fuzz (list (list int)) "should send elements in order, deconstructing one level of `List`" <|
                \xs ->
                    TList.reduce
                        (T.concat TList.stepper
                            |-> expect (List.concat xs)
                        )
                        xs
            ]
        , describe "reverse"
            [ fuzz (list int) "should reverse the elements passed to it" <|
                \xs ->
                    TList.reduce
                        (T.reverse
                            |-> T.take 1
                            |-> T.concat TList.stepper
                            |-> expect (List.reverse xs)
                        )
                        xs
            ]
        , describe "filter"
            [ fuzz (list int) "should filter out False values" <|
                \xs ->
                    let
                        predicate =
                            (\x -> x % 2 == 0)
                    in
                        TList.reduce (T.filter predicate |-> expect (List.filter predicate xs)) xs
            ]
        , describe "intersperse"
            [ fuzz (list int) "should put an extra element between each other element." <|
                \xs ->
                    TList.reduce (T.intersperse 0 |-> expect (List.intersperse 0 xs)) xs
            ]
        , describe "repeatedly"
            [ fuzz int "should just keep emitting whatever it was given until it receives `Halt`" <|
                \x ->
                    let
                        n =
                            128
                    in
                        (T.repeatedly |-> T.take n |-> expect (List.repeat n x)) T.cap (Just x) |> T.finish
            ]
        , describe "fold"
            [ fuzz (list int) "should fold values until it receives `Nothing`" <|
                \xs ->
                    let
                        sum =
                            List.sum xs
                    in
                        TList.reduce (T.fold (+) 0 |-> T.take 2 |-> expect ([ sum, sum ])) xs
            ]
        , describe "isEmpty"
            [ fuzz (list unit) "emits True on empty and False on non-empty" <|
                \xs ->
                    TList.reduce T.isEmpty xs
                        |> Expect.equal (List.isEmpty xs)
            ]
        , describe "length"
            [ fuzz (list unit) "emits a count of elements on Nothing" <|
                \xs ->
                    TList.reduce T.length xs
                        |> Expect.equal (List.length xs)
            ]
        , describe "member" <|
            [ fuzz2 int (list int) "upon ingesting `Nothing` emits `False` if the item has not been seen and `True` if it has." <|
                \x xs ->
                    TList.reduce (T.member x) xs
                        |> Expect.equal (List.member x xs)
            ]
        , describe "partition" <|
            [ fuzz (list int) "should sort and reduce items based on the predicate" <|
                \xs ->
                    let
                        predicate =
                            (\x -> x % 2 == 0)
                    in
                        xs
                            |> TList.reduce
                                (T.partition predicate
                                    (T.reverse T.cap)
                                    (T.length T.cap)
                                )
                            |> Expect.equal
                                (List.partition predicate xs
                                    |> (\( trues, falses ) -> ( List.reverse trues, List.length falses ))
                                )
            ]
        , describe "repeat"
            [ fuzz (list (tuple ( intRange 0 128, int ))) "should emit the value n times" <|
                \xs ->
                    let
                        repeats =
                            List.concatMap (uncurry List.repeat) xs

                        maybeXs =
                            List.map (\( n, x ) -> ( n, Just x )) xs
                    in
                        TList.reduce (T.repeat |-> expect repeats) maybeXs
            ]
        ]
