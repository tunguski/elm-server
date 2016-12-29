module Rest exposing (RestResult,
                      Rest,
                      RestConfig,
                      restCollection,
                      config,
                      ignoreError,
                      andThenReturn,
                      withHeader,
                      withQueryParams,
                      get,
                      findAll,
                      post,
                      postCommand,
                      put,
                      delete)

import Http exposing (Error)
import HttpBuilder exposing (..)
import Json.Decode as Json
import Json.Encode as JE
import Platform.Cmd as Cmd
import Result exposing (Result(..))
import String exposing (concat)
import Task exposing (Task)


type alias RestResult item =
    Result Error item


type Rest item =
    Rest (RestConfig item) (List (RequestBuilder item -> RequestBuilder item))


type alias RestConfig item =
    { baseUrl : String
    , collectionName : String
    , decoder : Json.Decoder item
    , encoder : item -> JE.Value
    }


restCollection : String -> String -> Json.Decoder item -> (item -> JE.Value) -> Rest item
restCollection baseUrl collectionName decoder encoder = 
    Rest (RestConfig baseUrl collectionName decoder encoder) []


config : Rest item -> RestConfig item
config rest =
    case rest of
        Rest config modifiers ->
            config


ignoreError : Task y a -> Task x a -> Task y a
ignoreError errorTask task =
    Task.onError (\error -> errorTask) task


andThenReturn : Task x b -> Task x a -> Task x b
andThenReturn fn task =
    Task.andThen (\result -> fn) task


withHeader : String -> String -> Rest item -> Rest item 
withHeader key value rest = 
    case rest of
        Rest config modifiers ->
            Rest config (modifiers ++ [ HttpBuilder.withHeader key value ])


withQueryParams : List (String, String) -> Rest item -> Rest item
withQueryParams params rest =
    case rest of
        Rest config modifiers ->
            Rest config (modifiers ++ [ HttpBuilder.withQueryParams params ])


apply : List (RequestBuilder item -> RequestBuilder item) -> RequestBuilder item -> RequestBuilder item 
apply modifiers builder =
    -- fold all modifiers
    List.foldl (\modifier b ->
        modifier b
    ) builder modifiers 


baseQuery : Rest item -> (RestConfig item -> RequestBuilder item -> RequestBuilder a) -> Task Error a
baseQuery rest custom =
  case rest of
    Rest config modifiers ->
      HttpBuilder.get (config.baseUrl ++ config.collectionName)
      |> withExpect (Http.expectJson config.decoder)
      |> apply modifiers
      |> custom config
      |> toTask


withMethod method builder =
    { builder | method = method }


subResource name builder =
    { builder | url = builder.url ++ "/" ++ name }


get : String -> Rest item -> Task Error item
get itemId rest =
  baseQuery rest (\config -> 
    subResource itemId)


findAll : Rest item -> Task Error (List item)
findAll rest =
    baseQuery rest 
        (withExpect << Http.expectJson << Json.list << .decoder) 


post : String -> item -> Rest item -> Task Error String
post itemId item rest =
    baseQuery rest (\config -> 
        withMethod "post"
        >> withJsonBody (config.encoder item)
        >> HttpBuilder.withHeader "Content-Type" "application/json"
        >> withExpect Http.expectString)


postCommand : String -> Rest item -> Task Error String
postCommand command rest =
    baseQuery rest (\config -> 
        subResource command
        >> withMethod "post"
        >> HttpBuilder.withHeader "Content-Type" "application/json"
        >> withExpect Http.expectString)


put : String -> item -> Rest item -> Task Error String
put itemId item rest =
    baseQuery rest (\config -> 
        subResource itemId
        >> withMethod "put"
        >> withJsonBody (config.encoder item)
        >> HttpBuilder.withHeader "Content-Type" "application/json"
        >> withExpect Http.expectString)


delete : String -> Rest item -> Task Error String
delete itemId rest =
    baseQuery rest (\config -> 
        subResource itemId
        >> withMethod "delete"
        >> withExpect Http.expectString)


