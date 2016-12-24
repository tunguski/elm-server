module UrlParse exposing (Parse(..), parse)

import String


type Parse fn
    = S (String -> List (Parse fn))
    | I (Int -> List (Parse fn))
    | P String (List (Parse fn))
    | T fn
    | F (() -> fn)


chooseFirst list input =
    case list of
        h :: t ->
            case internal h input of
                Ok result ->
                    Ok result

                _ ->
                    chooseFirst t input

        [] ->
            Err "No match found"


internal parser input =
    case parser of
        S function ->
            let
                splitted =
                    String.split "/" input
            in
                case splitted of
                    -- first part is "" because url started of "/"
                    _ :: h :: t ->
                        chooseFirst (function h) ("/" ++ (String.join "/" t))

                    _ ->
                        Err "No match found"

        I function ->
            let
                splitted =
                    String.split "/" input
            in
                case splitted of
                    -- first part is "" because url started of "/"
                    _ :: h :: t ->
                        case String.toInt h of
                            Ok int ->
                                chooseFirst (function int) ("/" ++ (String.join "/" t))

                            Err err ->
                                Err err

                    _ ->
                        Err "No match found"

        P path list ->
            if String.startsWith ("/" ++ path) input then
                chooseFirst list (String.dropLeft (String.length path + 1) input)
            else
                Err "no match found"

        T result ->
            case input of
                "" ->
                    Ok result

                "/" ->
                    Ok result

                _ ->
                    Err "No match found"

        F function ->
            case input of
                "" ->
                    Ok (function ())

                "/" ->
                    Ok (function ())

                _ ->
                    Err "No match found"


parse : Parse a -> String -> Result String a
parse parser input =
    internal parser input
