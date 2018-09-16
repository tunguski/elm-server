module Common exposing (isNothing)

import Maybe exposing (..)


isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True
