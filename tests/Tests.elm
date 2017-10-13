module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Transduction as Trans
import Transduction.Reply as Reply
import Transduction.Collection.List as TCList
import List.Extra


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

                    Err _ ->
                        fail "Received a failure state. This shouldn't happen."
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
                        (Trans.take n |> Trans.compose (expectReducer (List.take n xs)))
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
        ]


transducerSuite : Test
transducerSuite =
    describe "Transducers"
        [ describe "withIndex transducer"
            [ fuzz (list int) "should tag each element with its index" <|
                \xs ->
                    listReduce
                        (Trans.withIndex
                            |> Trans.compose (expectReducer (withIndex xs))
                        )
                        xs
            ]
        , describe "take transducer"
            [ fuzz2 int (list int) "should take (at most) the first n elements" <|
                \n xs ->
                    listReduce (Trans.take n |> Trans.compose (expectReducer (List.take n xs))) xs
            ]
        , describe "concat transducer"
            [ fuzz (list (list int)) "should send elements in order, deconstructing one level of `List`" <|
                \xs ->
                    listReduce
                        (Trans.concat TCList.stepper
                            |> Trans.compose (expectReducer (List.concat xs))
                        )
                        xs
            ]
        ]


withIndex : List a -> List ( Int, a )
withIndex xs =
    List.Extra.zip (List.range 0 (List.length xs)) xs
