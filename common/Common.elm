module Common exposing (..)


import Maybe exposing (..)


isNothing maybe =
    case maybe of
        Just _ -> False
        Nothing -> True


