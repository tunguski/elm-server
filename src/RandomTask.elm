module RandomTask exposing (..)


import Random
import Task


randomInt : Task x Int
randomInt =
  Time.now
    |> Task.map (\seed ->
      Random.step (Random.int Random.minInt Random.maxInt) seed
        |> fst
    )


