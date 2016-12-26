module MongoDb exposing (..)

import Http exposing (Error)
import HttpBuilder exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode as JE
import Platform.Cmd as Cmd
import Result exposing (Result(..))
import String exposing (concat)
import Task exposing (Task)
import BaseModel exposing (Collection, collectionDecoder)
import Rest exposing (..)


type CollectionUrl =
  CollectionUrl String String


collectionUrl : Rest item -> CollectionUrl
collectionUrl rest =
    let
        conf = config rest
    in
        CollectionUrl conf.baseUrl conf.collectionName


getCollection : CollectionUrl -> Task Error String
getCollection rest =
  case rest of
    CollectionUrl baseUrl collectionName ->
      HttpBuilder.get (baseUrl ++ collectionName)
      |> withExpect Http.expectString
      |> toTask


putCollection : CollectionUrl -> Task Error String
putCollection rest =
  case rest of
    CollectionUrl baseUrl collectionName ->
      HttpBuilder.put (baseUrl ++ collectionName)
      |> withExpect Http.expectString
      |> toTask


deleteCollection : CollectionUrl -> Task Error String
deleteCollection rest =
  case rest of
    CollectionUrl baseUrl collectionName ->
      HttpBuilder.delete (baseUrl ++ collectionName)
      |> withExpect Http.expectString
      |> toTask


createCollection rest =
    getCollection rest
    |> ignoreError (putCollection rest)


getString : String -> Rest item -> Task Error String
getString itemId rest =
    let
        conf = config rest
    in
        HttpBuilder.get (conf.baseUrl ++ conf.collectionName ++ "/" ++ itemId)
        |> withExpect Http.expectString
        |> toTask


listDocuments : Rest item -> Task Error (Collection item)
listDocuments rest =
    let
        conf = config rest
    in
        HttpBuilder.get (conf.baseUrl ++ conf.collectionName)
        |> withExpect (Http.expectJson (collectionDecoder conf.decoder))
        |> toTask


type alias MongoDb =
    { name : String
    , description : Maybe String
    , collections : List String
    }


mongoDbDecoder : Decoder MongoDb
mongoDbDecoder =
    Json.map3 MongoDb
        (field "_id" Json.string)
        (maybe (field "desc" Json.string))
        (at [ "_embedded", "rh:coll" ] <| (Json.list (field "_id" Json.string)))


