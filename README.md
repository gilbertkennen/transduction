# Transduction
Transducers in Elm

# Example

```Elm
import Transduction exposing (reduce, compose)
import Transduction.Transducers as Transducers
import Transduction.Collection.List as Tlist

somethingCrazy : List Int -> List Int
somethingCrazy xs =
    reduce
        ( TList.emitter xs
            |> compose (Transducers.map (\x -> List.range 1 x))
            |> compose (Transducers.concat TCList.emitter)
            |> compose (Transducers.filter (\x -> x % 2 == 0))
            |> compose (Transducers.take 3)
            |> compose (TList.reducer)
        )
```

This transducer does a lot of things, mostly to show the sorts of things you can do (it might get wilder as more transducers are implemented). To be clear about what is happening, let's say we call:

```Elm
somethingCrazy [3, 7, 9, 87, 2^500 + 1]
```

When we run `reduce`, `TList.emitter` will 'emit' (send down the transducer) the first value of `3`.
`map` will then turn that into `[1, 2, 3]` and emit it.
`concat` (armed with an emitter for lists) will send the elements of the list it was given down one at a time, beginning with `1`.
`filter` then checks if it is even and deciding not, does not emit, and just returns back up the transducer stack until something stops it.
`concat` still has elements to emit, so it emits `2`.
`filter` then checks and finds that it is even and emits `2`.
`take` checks and sees that it is still willing to emit three more elements, so it emits `2`.
`TList.reducer` takes the `2` and updates its state then returns back up the call stack.
Speeding up a bit, `concat` grabs the reply and emits `3` which gets filtered out.
Now `concat` is out of elements, so it lets the reply bubble back up until `TList.emitter` finds it has more to give, so it emits `7`.
This turns into `[1, 2, 3, 4, 5, 6, 7]`, `1` gets filtered out, `2` is kept, `3` is filtered out, `4` is kept, but now `take` is out of elements, so it halts further reduction all the way up.
In the end, we are left with `[2, 2, 4]`.

# Terminology

* `Transducer` is the overarching term for these composable components.
* `Emitter` is a type of transducer which doesn't take (meaningful) emitted values, just emits them when given a `()`.
* `Reducer` is a type of transducer which doesn't emit values, just accepts them.
* `Reduction` is a type of transducer which neither takes nor emits values, it is ready to be run.

As long as the types don't conflict, you can compose transducers any way you like, but in the end you need to make a `Reduction` which eventually means composing an `Emitter` to the beginning and a `Reducer` to the end.
