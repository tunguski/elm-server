module RandomTask exposing (randomIdentifier, randomInt)

import Random
import Task exposing (..)
import Time
import Tuple


randomInt : Task x Int
randomInt =
    Time.now
        |> Task.map
            (\seed ->
                Random.step (Random.int Random.minInt Random.maxInt) (Random.initialSeed <| floor seed)
                    |> Tuple.first
            )


randomIdentifier : Task x String
randomIdentifier =
    Time.now
        |> Task.map
            (\seed ->
                Random.step
                    (Random.list 8 <| Random.int 0 15)
                    (Random.initialSeed <| floor seed)
                    |> (\( result, seed ) ->
                            result
                                |> List.map
                                    (\i ->
                                        if i < 10 then
                                            toString i

                                        else
                                            "abcdef"
                                                |> String.dropLeft (i - 10)
                                                |> String.dropRight (15 - i)
                                    )
                                |> String.join ""
                       )
            )
