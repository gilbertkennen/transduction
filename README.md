# Transduction
Transducers in Elm

# Example

```Elm
import Transduction.Transducers as Transducers exposing (compose)
import Transduction.Collection.List as Tlist

somethingCrazy : List Int -> List Int
somethingCrazy xs =
    Transducers.transduce
        ( Transducers.concat TList.stepper
            |> compose (Transducers.mapInput (\x -> List.range 1 x))
            |> compose (Transducers.concat TList.stepper)
            |> compose (Transducers.filter (\x -> x % 2 == 0))
            |> compose (Transducers.take 3)
            |> compose (Transducers.fold (::) [])
            |> compose (Transducers.mapInput List.reverse)
        )
        xs
```

This transducer does a lot of things, mostly to show the sorts of things you can do (it might get wilder as more transducers are implemented). To be clear about what is happening, let's say we call:

```Elm
somethingCrazy [3, 7, 9, 87, 2^500 + 1]
```

The part in parentheses is defining the transducer itself. The first step is at the top and the last step at the bottom.
`concat` takes a stepper which knows about a particular sort of collection. When it receives that collection (in this case a `List`) it will emit elements from it one-by-one until it is empty. We will eventually pass in `xs`, so let's just follow that. The first value it emits is `3`.
`mapInput` will then turn that into `[1, 2, 3]` and emit it.
`concat` (armed with a stepper for `List`) will send the elements of the list it was given down one at a time, beginning with `1`.
`filter` then checks if the value is even and deciding not, does not emit. It returns back up the transducer stack until something stops it.
`concat` still has elements to emit, so it emits `2`.
`filter` then checks and finds that it is even, which it is, and emits `2`.
`take` checks and sees that it is still willing to emit three more elements, so it emits `2`.
`fold` appends the `2` to its `[]` and returns back up the stack.
Speeding up a bit, `concat` grabs the reply and emits `3` which gets filtered out.
Now `concat` is out of elements, so it lets the reply bubble back up until the first `concat` finds it has more to give, so it emits `7`.
This turns into `[1, 2, 3, 4, 5, 6, 7]`, `1` gets filtered out, `2` is kept, `3` is filtered out, `4` is kept, but now `take` is out of elements. It finishes and halts.
When `fold` sees a `Nothing` it emits its current state, in this case `Just [4, 2, 2]`.
`mapInput` applies `List.reverse` and emits `Just [2, 2, 4]`
After we get here, the final step is to cap off the end of the `Transducer` with `Transducers.last`. This will halt the first time it recieves a value and sends that value back up wrapped in `Just`. This transducer is so simple that `transduce` automatically appends it.
In the end, we are left with `Just [2, 2, 4]`.

# Terminology

* `Reducer` - A data type which can either reduce or finish. You can't make these from scratch, but you can wrap them with a `Transducer`.
* `Transducer` - A function of `Reducer reducerInput reducerOutput -> Reducer thisInput thisOutput`. It wraps a `Reducer` producing a new one.
* ingest - accept a value from outside.
* emit - pass a value to an inner `Reducer`

As long as the types don't conflict, you can compose transducers any way you like, but as there is only one base `Reducer`, to run a `Transducer` it must be able to accept it.

# Discussion

Since I can find only a little bit of conversation about this out in the world (please feel free to let me know about things, I might not have seen them) I will explain in some detail what I think transducers are and what my exploration has revealed to me.

Transducers were designed to solve particular problems in a particular language, Clojure. They rest on the realization that if you remove the unpacking and packing steps of a fold (a.k.a. reduce), the bits in the middle are collection agnostic. If those bits could be composed as easily as functions and then have the beginning and end added later, then we could have something which looks just like a fold, but all of the bits in the middle wouldn't need to be re-implemented for everything.

A transducer wraps a reducer providing extra functionality. It has the ability to transform elements ingested, can choose whether to emit values or not, and can terminate further reduction. Many important transducers require maintaining state and because Clojure has side-effects, they have a way of cleaning up when reduction is complete.

I will be talking mostly about reducers, because that is the end-goal of a transducer. The actual transducer is some data structure which can be combined with a reducer to make a new wrapped reducer.

I have yet to see a transducer implementation properly handle early-termination, but I have seen implementations of state in languages without side-effects. They recognize that the reducer state could be used to hold the state for any transducers as well. Here is a description of a reducer which I think most closely mirrors Clojure's.

``` Elm
type Reducer r r_ a b
    = Reducer
        (Maybe r_)
        (r_ -> r)
        (a -> r -> r)
        (r -> b)
```

`Maybe r_` lets us have our optional base-reducer initial value. I have never seen this implemented as it is an artifact of Clojure being a language which uses default arguments quite often.
`r_ -> r` lets us wrap the base-reducer state into the transducer state.
`a -> r -> r` is our reducing function.
`r -> b` strips out all of the transducer state and performs last-minute manipulations of data.

The first realization is that we have two ways that state gets into our reduction. There is the actual `r_` which is the state for the reducer, but all of the rest of the state lives in `r_ -> r`. Why are we giving the base reducer special treatment here? The state from the transducers comes when we call the function which constructs the transducer, why should reducers be any different?

``` Elm
type Reducer r a b
    = Reducer
        r
        (a -> r -> r)
        (r -> b)
```

Next, let's consider what a fold function which works on reducers might look like.

``` Elm
foldList : Reducer r a b -> List a -> b
```

The state value ends up not being useful to anyone outside of the implementation. This is a classic implementation detail which we should probably hide. There are various ways to do this in different languages, but Elm uses a type system with few luxuries, so the most straight-forward way is to use currying.

Let's pre-load the state into each of those functions and instead of the reducer returning state, it just returns a whole new `Reducer` with the new state built-in.

``` Elm
type Reducer a b
    = Reducer
        (a -> Reducer a b)
        b
```

This looks pretty simple, but we still have the pesky problem of early-termination which hasn't been dealt with. We actually have a fairly elegant solution to that now. If a `Reducer` shouldn't allow new elements to be emitted, just don't construct the function to allow it.

``` Elm
type Reducer a b
    = Reducer
        (Maybe (a -> Reducer a b))
        b

type alias Transducer b d a c =
    Reducer b d -> Reducer a c
```

Let's implement some things.

``` Elm
listReducer : List a -> Reducer a (List a)
listReducer xs =
    Reducer
        (Just (\x -> listReducer (x :: xs)))
        (List.reverse xs)
```

``` Elm
mapValue : (a -> b) -> Transducer b c a c
mapValue f (Reducer maybeReduceF output) =
    Reducer
        (Maybe.map (\reduceF x -> mapValue f (reduceF (f x))) maybeReduceF)
        output
```

``` Elm
take : Int -> Transducer a b a b
take n (Reducer maybeReduceF output) =
    Reducer
        (if n <= 0 then
            Nothing
        else
            Maybe.map
                (\reduceF x ->
                    let
                        (Reducer maybeNewReduceF newOutput) =
                            reduceF x
                    in
                        if n == 1 then
                            Reducer Nothing newOutput
                        else
                            take (n - 1) (Reducer maybeNewReduceF newOutput)
                )
                maybeReduceF
        )
        output
```

Large sections of this are very repetitive between transducers and the actual implementations look a lot less gnarly.

Another thing to notice is that any reducer can be turned into a transducer by taking and ignoring the reducer value.

``` Elm
listReducer : List a -> Transducer x y a (List a)
listReducer xs _ =
    Reducer
        (Just (\x -> listReducer (x :: xs)))
        (List.reverse xs)
```

They are all a single type. Now we just need to define the most basic reducer.

``` Elm
unit : Reducer a ()
unit =
    Reducer Nothing ()
```

But that's not quite *everything*, we still have the problem that something needs to emit elements from the collection. Although, we might actually want that in transducer form as well.

``` Elm
concatList : Transducer a b (List a) b
concatList (Reducer maybeReduceF output) =
    Reducer
        (concatListHelper (Reducer maybeReduceF output))
        output


concatListHelper : Reducer a b -> List a -> Reducer a b
concatListHelper (Reducer maybeReduceF output) xs =
    case xs of
        [] ->
            Reducer (concatList maybeReduceF) output

        x :: rest ->
            case maybeReduceF of
                Nothing ->
                    Reducer Nothing output

                Just reduceF ->
                    concatListHelper (reduceF x) rest

```

If we put this on the top of a transducer stack, then we have our emitting done as well. Of course, this also works as a normal concat which takes potentially multiple lists.

It would be nice to have a function to bring it all together.

``` Elm
transduce : Transducer x () a b -> a -> b
transduce transducer x =
    let
        (Reducer maybeReduceF output) =
            transducer unit
    in
        Maybe.map
            (\reduceF ->
                let
                    (Reducer _ newOutput) =
                        reduceF x
                in
                    newOutput
            )
            |> Maybe.withDefault output
```

Any transducer which can take the `unit` reducer can be transformed into a simple function.

I'll probably continue to noodle on this and might add to it, but I thought there was enough here to share.
