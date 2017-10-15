module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Transduction as T
import Transduction.Transducers as Trans
import Transduction.Reply as Reply
import Transduction.Collection.List as TCList
import List.Extra


(|->) :
    T.Transducer betweenState betweenInput betweenResult thisState thisInput thisResult
    -> T.Transducer afterState afterInput afterResult betweenState betweenInput betweenResult
    -> T.Transducer afterState afterInput afterResult thisState thisInput thisResult
(|->) =
    flip T.compose
infixr 8 |->


listReduce : T.Reducer state input result -> List input -> result
listReduce =
    T.reduce TCList.stepper


expectReducer : List input -> T.Reducer (Result String (List input)) input Expect.Expectation
expectReducer xs =
    T.reducer
        (Reply.continue (Ok xs))
        (\x state ->
            let
                fail =
                    Reply.halt << Err
            in
                case state of
                    Ok [] ->
                        fail <| "Tried to consume " ++ toString x ++ " but list is empty."

                    Ok (y :: rest) ->
                        if x == y then
                            Reply.continue (Ok rest)
                        else
                            fail <| "Was given " ++ toString x ++ " but expected " ++ toString y

                    Err err ->
                        fail err
        )
        (\state ->
            case state of
                Ok [] ->
                    Expect.pass

                Ok rem ->
                    Expect.fail ("Did not consume: " ++ toString rem)

                Err err ->
                    Expect.fail err
        )


stepperSuite : Test
stepperSuite =
    describe "Steppers"
        [ describe "list stepper"
            [ fuzz (list int) "should emit elements in order" <|
                \xs ->
                    xs
                        |> T.reduce TCList.stepper (expectReducer xs)
            , fuzz2 int (list int) "should stop early when halted" <|
                \n xs ->
                    xs
                        |> T.reduce TCList.stepper
                            (Trans.take n |-> expectReducer (List.take n xs))
            ]
        ]


reducerSuite : Test
reducerSuite =
    describe "Reducers"
        [ describe "list reducer"
            [ fuzz (list int) "returns a list of elements in order" <|
                \xs ->
                    xs
                        |> listReduce TCList.reducer
                        |> Expect.equal xs
            ]
        , describe "isEmpty reducer" <|
            [ fuzz (list unit) "returns True on empty and False on non-empty" <|
                \xs ->
                    xs
                        |> listReduce Trans.isEmpty
                        |> Expect.equal (List.isEmpty xs)
            ]
        , describe "length reducer"
            [ fuzz (list unit) "returns a count of elements" <|
                \xs ->
                    xs
                        |> listReduce Trans.length
                        |> Expect.equal (List.length xs)
            ]
        , describe "member reducer" <|
            [ fuzz2 int (list int) "should return true if it is a member and false if not" <|
                \x xs ->
                    xs
                        |> listReduce (Trans.member x)
                        |> Expect.equal (List.member x xs)
            ]
        , describe "partition reducer" <|
            [ fuzz (list int) "should sort and reduce items based on the predicate" <|
                \xs ->
                    let
                        predicate =
                            (\x -> x % 2 == 0)
                    in
                        xs
                            |> listReduce
                                (Trans.partition predicate
                                    (Trans.reverse |-> TCList.reducer)
                                    Trans.length
                                )
                            |> Expect.equal
                                (List.partition predicate xs
                                    |> (\( trues, falses ) -> ( List.reverse trues, List.length falses ))
                                )
            ]
        ]


transducerSuite : Test
transducerSuite =
    describe "Transducers"
        [ describe "map transducer"
            [ fuzz (list int) "should map the values of the collection" <|
                \xs ->
                    let
                        f =
                            (+) 1

                        reducer =
                            expectReducer (List.map f xs)
                    in
                        listReduce (Trans.map f |-> reducer) xs
            ]
        , describe "statefulMap transducer"
            [ fuzz (list int) "should map stateful values of the collection" <|
                \xs ->
                    listReduce
                        (Trans.statefulMap 0 (\x prev -> ( x + prev, x ))
                            |-> expectReducer
                                    (List.take 1 xs ++ List.map2 (+) xs (List.drop 1 xs))
                        )
                        xs
            ]
        , describe "withIndex transducer"
            [ fuzz (list int) "should tag each element with its index" <|
                \xs ->
                    listReduce
                        (Trans.withIndex
                            |-> expectReducer (withIndex xs)
                        )
                        xs
            ]
        , describe "take transducer"
            [ fuzz2 int (list int) "should take (at most) the first n elements" <|
                \n xs ->
                    listReduce (Trans.take n |-> expectReducer (List.take n xs)) xs
            ]
        , describe "drop transducer"
            [ fuzz2 int (list int) "should skip the first n elements" <|
                \n xs ->
                    listReduce (Trans.drop n |-> expectReducer (List.drop n xs)) xs
            ]
        , describe "concat transducer"
            [ fuzz (list (list int)) "should send elements in order, deconstructing one level of `List`" <|
                \xs ->
                    listReduce
                        (Trans.concat TCList.stepper
                            |-> expectReducer (List.concat xs)
                        )
                        xs
            ]
        , describe "reverse transducer"
            [ fuzz (list int) "should reverse the elements passed to it" <|
                \xs ->
                    listReduce (Trans.reverse |-> expectReducer (List.reverse xs)) xs
            ]
        , describe "filter"
            [ fuzz (list int) "should filter out False values" <|
                \xs ->
                    let
                        filterF =
                            (\x -> x % 2 == 0)
                    in
                        listReduce (Trans.filter filterF |-> expectReducer (List.filter filterF xs)) xs
            ]
        , describe "intersperse"
            [ fuzz (list int) "should put an extra element between each other element." <|
                \xs ->
                    listReduce (Trans.intersperse 0 |-> expectReducer (List.intersperse 0 xs)) xs
            ]
        ]


withIndex : List a -> List ( Int, a )
withIndex xs =
    List.Extra.zip (List.range 0 (List.length xs)) xs
