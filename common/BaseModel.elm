module BaseModel exposing (..)

import Date exposing (Date, fromString, fromTime)
import Result exposing (toMaybe)
import Json.Decode as Json exposing (..)
import Json.Encode as JE


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
                            (at [ "_embedded", "rh:doc" ] <| Json.list itemDecoder)
                )
        )


listDecoder : Decoder item -> Decoder (List item)
listDecoder =
    Json.list


listToValue encoder list =
    JE.list (List.map encoder list)


encodeCollection encoder collection =
    JE.encode 0 <| listToValue encoder collection.elements


encode encoder item =
    JE.encode 0 <| encoder item


maybeEncodeDate maybe =
    case maybe of
        Just date ->
            JE.float <| Date.toTime date

        Nothing ->
            JE.null


dateParser : Float -> Date
dateParser input =
    fromTime input


