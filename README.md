# Transduction
Transducers in Elm

These transducers are designed to produce functions of `Maybe input -> Reply input output`. No special reduction function is required. `input` can be a collection and a transducer will handle tearing it apart.

# Example

```Elm
import Transduction exposing (compose)
import Transduction.Collection.List as Tlist

somethingCrazy : List Int -> List Int
somethingCrazy xs =
        ( TList.concat TList.stepper
            |> compose (Transduction.mapInput (\x -> List.range 1 x))
            |> compose (Transduction.concat TCList.stepper)
            |> compose (Transduction.filter (\x -> x % 2 == 0))
            |> compose (Transduction.take 3)
            |> compose (Transduction.fold (::) [])
            |> compose (Transduction.mapInput List.reverse)
        )
        Transduction.cap
        ( Just xs )
        |> Transduction.finish
```

This transducer does a lot of things, mostly to show the sorts of things you can do (it might get wilder as more transducers are implemented). To be clear about what is happening, let's say we call:

```Elm
somethingCrazy [3, 7, 9, 87, 2^500 + 1]
```

The part in parentheses is defining the transducer itself. The first step is at the top and the last step at the bottom.
`concat` takes a stepper which knows about a particular sort of collection. When it recieves that collection (in this case a `List`) it will emit elements from it one-by-one until it is empty. We will eventually pass in `Just xs`, so let's just follow that. The first value it emits is `Just 3`.
`mapInput` will then turn that into `Just [1, 2, 3]` and emit it.
`concat` (armed with a stepper for `List`s) will send the elements of the list it was given down one at a time, beginning with `1`.
`filter` then checks if the value is even and deciding not, does not emit. It returns back up the transducer stack until something stops it.
`concat` still has elements to emit, so it emits `Just 2`.
`filter` then checks and finds that it is even, which it is, and emits `Just 2`.
`take` checks and sees that it is still willing to emit three more elements, so it emits `Just 2`.
`fold` appends the `2` to its `[]` and returns back up the stack.
Speeding up a bit, `concat` grabs the reply and emits `3` which gets filtered out.
Now `concat` is out of elements, so it lets the reply bubble back up until the first `concat` finds it has more to give, so it emits `7`.
This turns into `[1, 2, 3, 4, 5, 6, 7]`, `1` gets filtered out, `2` is kept, `3` is filtered out, `4` is kept, but now `take` is out of elements. It emits `Nothing`
When `fold` sees a `Nothing` it emits its current state, in this case `Just [4, 2, 2]`.
`mapInput` applies `List.reverse` and emits `Just [2, 2, 4]`
After we get here, the final step is to cap off the end of the `Transducer` with `Transduction.cap`. This will halt the first time it recieves a value and sends that value back up.
In the end, we are left with `Halt [2, 2, 4]`.

# Terminology

* `Reducer` - A function of `Maybe input -> Reply input output`
* `Transducer` - A function of `Reducer afterInput afterOutput -> Reducer thisInput thisOutput`. It wraps a `Reducer` producing a new one.
* ingest - accept a value from outside.
* emit - pass a value to an inner `Reducer`

As long as the types don't conflict, you can compose transducers any way you like, but in the end you need to make a `Reducer` which eventually means composing an `Emitter` to the beginning and a `Reducer` to the end.
