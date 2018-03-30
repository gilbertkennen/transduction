module Queue exposing (Queue, empty, pop, push, append)


type Queue a
    = Queue (List a) (List a)


pop : Queue a -> Maybe ( a, Queue a )
pop (Queue input output) =
    case output of
        [] ->
            case input of
                [] ->
                    Nothing

                _ ->
                    pop (Queue [] (List.reverse input))

        x :: xs ->
            Just ( x, Queue input xs )


push : a -> Queue a -> Queue a
push x (Queue input output) =
    Queue (x :: input) output


append : Queue a -> Queue a -> Queue a
append (Queue inputL outputL) (Queue inputR outputR) =
    Queue inputL (List.concat [ outputL, List.reverse inputR, outputR ])


empty : Queue a
empty =
    Queue [] []
