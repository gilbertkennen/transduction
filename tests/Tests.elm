module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Transduction as Trans
import Transduction.Transducers as T
import Transduction.List as TList
import Transduction.Maybe as TMaybe


(|->) :
    Trans.Transducer betweenInput betweenResult thisInput thisResult
    -> Trans.Transducer afterInput afterResult betweenInput betweenResult
    -> Trans.Transducer afterInput afterResult thisInput thisResult
(|->) =
    flip Trans.compose
infixr 8 |->


expect : List input -> Trans.Transducer Never (Maybe Never) input Expect.Expectation
expect xs =
    Trans.transducer
        (\x reducer ->
            case xs of
                [] ->
                    Trans.halt <| Expect.fail ("Tried to consume " ++ toString x ++ " but list is empty.")

                y :: rest ->
                    if x == y then
                        expect rest reducer
                    else
                        Trans.halt <|
                            Expect.fail ("Was given " ++ toString x ++ " but expected " ++ toString y)
        )
        (\reducer ->
            case xs of
                [] ->
                    Expect.pass

                _ ->
                    Expect.fail ("Did not consume: " ++ toString xs)
        )



-- basicsSuite : Test
-- basicsSuite =
--     describe "Basics"
--         []


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
                        TList.transduce
                            (T.mapInput f
                                |-> expect (List.map f xs)
                            )
                            xs
            ]
        , describe "take"
            [ fuzz2 int (list int) "should take (at most) the first n elements" <|
                \n xs ->
                    TList.transduce
                        (T.take n
                            |-> expect (List.take n xs)
                        )
                        xs
            ]
        , describe "drop"
            [ fuzz2 int (list int) "should skip the first n elements" <|
                \n xs ->
                    TList.transduce
                        (T.drop n
                            |-> expect (List.drop n xs)
                        )
                        xs
            ]
        , describe "concat"
            [ fuzz (list (list int)) "should send elements in order, deconstructing one level of `List`" <|
                \xs ->
                    TList.transduce
                        (T.concat TList.stepper
                            |-> expect (List.concat xs)
                        )
                        xs
            ]
        , describe "reverse"
            [ fuzz (list int) "should reverse the elements passed to it" <|
                \xs ->
                    TList.transduce
                        (T.reverse
                            |-> T.concat TList.stepper
                            |-> expect (List.reverse xs)
                        )
                        xs
            ]
        , describe "filter"
            [ fuzz (list int) "should filter out `False` values" <|
                \xs ->
                    let
                        predicate =
                            (\x -> x % 2 == 0)
                    in
                        TList.transduce
                            (T.filter predicate
                                |-> expect (List.filter predicate xs)
                            )
                            xs
            ]
        , describe "intersperse"
            [ fuzz (list int) "should put an extra element between each other element." <|
                \xs ->
                    TList.transduce
                        (T.intersperse 0
                            |-> expect (List.intersperse 0 xs)
                        )
                        xs
            ]
        , describe "repeatedly"
            [ fuzz int "should just keep emitting whatever it was given until it receives `Halt`" <|
                \x ->
                    let
                        n =
                            128
                    in
                        T.transduce
                            (T.repeatedly
                                |-> T.take n
                                |-> expect (List.repeat n x)
                            )
                            x
            ]
        , describe "fold"
            [ fuzz (list int) "should fold values until it finishes" <|
                \xs ->
                    let
                        sum =
                            List.sum xs
                    in
                        TList.transduce
                            (T.fold (+) 0
                                |-> expect ([ sum ])
                            )
                            xs
            ]
        , describe "isEmpty"
            [ fuzz (list unit) "emits True on empty and False on non-empty" <|
                \xs ->
                    TList.transduce
                        (T.isEmpty
                            |-> expect [ List.isEmpty xs ]
                        )
                        xs
            ]
        , describe "length"
            [ fuzz (list unit) "emits a count of elements on finish" <|
                \xs ->
                    TList.transduce
                        (T.length
                            |-> expect [ List.length xs ]
                        )
                        xs
            ]
        , describe "member" <|
            [ fuzz2 int (list int) "emits `True` and `Halt`s if it receives the value and emits `False` on finish" <|
                \x xs ->
                    TList.transduce
                        (T.member x
                            |-> expect [ List.member x xs ]
                        )
                        xs
            ]
        , describe "partition" <|
            [ fuzz (list int) "should sort and reduce items based on the predicate" <|
                \xs ->
                    let
                        predicate =
                            (\x -> x % 2 == 0)
                    in
                        TList.transduce
                            (T.partition predicate
                                (T.reverse |-> T.withDefault [])
                                (T.length |-> T.withDefault 0)
                                |-> expect
                                        [ ( List.reverse (List.filter predicate xs)
                                          , List.length (List.filter (not << predicate) xs)
                                          )
                                        ]
                            )
                            xs
            ]
        , describe "repeat"
            [ fuzz (list (tuple ( intRange 0 128, int ))) "should emit the value n times" <|
                \xs ->
                    let
                        repeats =
                            List.concatMap (uncurry List.repeat) xs
                    in
                        TList.transduce (T.repeat |-> expect repeats) xs
            ]
        , describe "mapOutput"
            [ fuzz (maybe int) "should apply the map function to the output" <|
                \maybeX ->
                    T.transduce
                        (TMaybe.maybe |-> T.mapOutput (Maybe.map ((+) 1)))
                        maybeX
                        |> Expect.equal (Maybe.map ((+) 1) maybeX)
            ]
        , describe "withDefault"
            [ fuzz (maybe int) "should fix `Nothing` output values" <|
                \maybeX ->
                    T.transduce
                        (TMaybe.maybe |-> T.withDefault 0)
                        maybeX
                        |> Expect.equal (Maybe.withDefault 0 maybeX)
            ]
        ]
