module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Transduction as Trans
import Transduction.Reply as Reply
import Transduction.Collection.List as TCList
import List.Extra


(|->) :
    Trans.Transducer betweenState betweenInput betweenResult thisState thisInput thisResult
    -> Trans.Transducer afterState afterInput afterResult betweenState betweenInput betweenResult
    -> Trans.Transducer afterState afterInput afterResult thisState thisInput thisResult
(|->) =
    flip Trans.compose
infixr 8 |->


listReduce : Trans.Reducer state input result -> List input -> result
listReduce =
    Trans.reduce TCList.stepper


expectReducer : List input -> Trans.Reducer (Result String (List input)) input Expect.Expectation
expectReducer xs =
    Trans.reducer
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
                    Trans.reduce TCList.stepper (expectReducer xs) xs
            , fuzz2 int (list int) "should stop early when halted" <|
                \n xs ->
                    Trans.reduce TCList.stepper
                        (Trans.take n |-> expectReducer (List.take n xs))
                        xs
            ]
        ]


reducerSuite : Test
reducerSuite =
    describe "Reducers"
        [ describe "list reducer"
            [ fuzz (list int) "returns a list of elements in order" <|
                \xs ->
                    listReduce TCList.reducer xs
                        |> Expect.equal xs
            ]
        , describe "length reducer"
            [ fuzz (list unit) "returns a count of elements" <|
                \xs ->
                    xs
                        |> listReduce Trans.length
                        |> Expect.equal (List.length xs)
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
                    in
                        listReduce
                            (Trans.map f |-> expectReducer (List.map f xs))
                            xs
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
        ]


withIndex : List a -> List ( Int, a )
withIndex xs =
    List.Extra.zip (List.range 0 (List.length xs)) xs
