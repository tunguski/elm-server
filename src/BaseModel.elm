module BaseModel exposing (Collection, collectionDecoder, encode, encodeCollection, listDecoder, listToValue, longFloat, longInt, longPosix, maybeEncodeDate)

import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Result exposing (toMaybe)
import Time exposing (Posix)


type alias Collection item =
    { name : String
    , description : Maybe String
    , elements : List item
    }


collectionDecoder : Decoder item -> Decoder (Collection item)
collectionDecoder itemDecoder =
    Json.map3 Collection
        (field "_id" Json.string)
        (maybe <| field "desc" Json.string)
        (field "_returned" Json.int
            |> andThen
                (\count ->
                    case count of
                        0 ->
                            succeed []

                        _ ->
                            at [ "_embedded", "rh:doc" ] <| Json.list itemDecoder
                )
        )


listDecoder : Decoder item -> Decoder (List item)
listDecoder =
    Json.list


listToValue : (item -> JE.Value) -> List item -> JE.Value
listToValue =
    JE.list


encodeCollection encoder collection =
    JE.encode 0 <| listToValue encoder collection.elements


encode encoder item =
    JE.encode 0 <| encoder item


parse parser value =
    parser value
        |> Maybe.map (\s -> succeed s)
        |> Maybe.withDefault (fail value)


longInt =
    oneOf
        [ int
        , field "$numberLong" (andThen (parse String.toInt) string)
        , string |> andThen (parse String.toInt)
        ]


longPosix =
    andThen (Time.millisToPosix >> succeed) int


longFloat =
    oneOf
        [ float
        , field "$numberLong" (andThen (parse String.toFloat) string)
        ]


maybeEncodeDate maybe =
    case maybe of
        Just date ->
            JE.float <| toFloat <| Time.posixToMillis date

        Nothing ->
            JE.null
