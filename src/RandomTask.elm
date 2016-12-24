module RandomTask exposing (..)

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
