# Transduction
Transducers in Elm

# Yet Another Major Rewrite

Given that this is more of a passion project than anything, I was learning some stuff and had ideas on how to rewrite these transducers again. It ends up I couldn't get all of the type safety that I might like, but it is still a very fun and interesting implementation, if I say so myself. I have not fully ported things over yet and am unsure on which transducers to implement from here forward.

Building on my previous ideas about adequately powerful transducers being more like a pipe than like an onion, it finally occurred to me that perhaps we don't even need a concept of reducers. Instead, we have an in-wire and an out-wire and we can push elements in and, occasionally, get elements out. We still end up with pipes of transformation, but now we don't have to worry about injecting some sort of reducer (which I had already realized there could be just one generic reducer) and without worrying about propagating the result back up through the layers of transducer.

This also means that construction of even the most feature-rich transducers doesn't need to know anything about how they will be stitched together, something which made the previous version more difficult to understand. This has also resulted in a simpler `Transducer` type function.

# Example Time

``` Elm
import Transduction exposing (compose, push, terminateSignal, pop)
import Transduction.Transducers exposing (elementsOf, take, toList)

takeWithTransducers : Int -> List a -> List a
takeWithTransducers n xs =
    ( elementsOf |> compose (take n) |> compose toList )
        |> push xs
        |> terminateSignal
        |> pop
        |> Maybe.withDefault []
```

We are composing together three transducers (`elementsOf`, `take`, and `toList`) to make our main transducer which is now of type `Transducer (List a) (List a)` (a transducer that uses `List a` on its in-wire and produces `List a` on its out wire). Next, we push our `List a` into the transducer. This data will flow through in the following way.

- `elementsOf : Transducer (List a) a` takes a list and then produces each element, which is a very convenient way to `push` many items at once.
- `take : Int -> Transducer a a` takes elements and counts down its value for each one. If it ever reaches zero, it terminates, which forgets transducers to the left and propagates a termination signal to the right.
- `toList` takes elements and saves them until it gets a termination signal upon which it will produce a list containing all of the saved elements.

Since `toList` won't produce until it receives a terminate signal and `take` might not have terminated yet (e.g. `take 5 [1, 2]`), we send our own `terminateSignal` to force things to complete.

Now we can `pop` the list off of the out wire. Termination doesn't propagate while the transducer to the right has something on its out-wire, so we can still pop after sending a termination. Although in this case it will always succeed, `pop` can possibly fail if there is nothing on the out-wire, so it produces a `Maybe` value which we have to handle.

Almost all of this code is actually very generic for handling the list input and the list output and could be pulled out into its own function.

```
import Transduction exposing (compose, push, terminateSignal, pop)
import Transduction.Transducers exposing (elementsOf, take, toList, map)

transduceLists : Transducer a b -> List a -> List b
transduceLists transducer xs =
    ( elementsOf |> compose transducer |> compose toList )
        |> push xs
        |> terminateSignal
        |> pop
        |> Maybe.withDefault []


takeWithTransducers : Int -> List a -> List a
takeWithTransducers n xs =
    transduceLists (take n) xs

mapWithTransducers : (a -> b) -> List a -> List b
mapWithTransducers f xs =
    transduceLists (map f) xs
```

Since they are designed to handle infinite streams, these transducers are lazy. They don't even try to do work until an attempt is made to read from them and stop as soon as another element has been produced, a termination propagates all the way, or all transducers are waiting for input.

If you did weld an infinite stream to the input of a transducer, it would result in a new potentially infinite stream.

The down-side to this implementation is that it has a bit more overhead than simply passing reducing functions directly.

The few transducers I have made from the primitives have felt a lot cleaner than previous implementations. Getting rid of reducers entirely has made things so much nicer.
