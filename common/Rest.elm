module Rest exposing (..)

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
  Rest String String (Json.Decoder item) (item -> JE.Value)


restCollection : String -> String -> Json.Decoder item -> (item -> JE.Value) -> Rest item
restCollection = Rest


ignoreError : Task y a -> Task x a -> Task y a
ignoreError errorTask task =
    Task.onError (\error -> errorTask) task


andThenReturn : Task x b -> Task x a -> Task x b
andThenReturn fn task =
    Task.andThen (\result -> fn) task


get : String -> Rest item -> Task Error item
get itemId rest =
  case rest of
    Rest baseUrl collectionName decoder encoder ->
      HttpBuilder.get (baseUrl ++ collectionName ++ "/" ++ itemId)
      |> withExpect (Http.expectJson decoder)
      |> toTask
--    Http.get (Debug.log "url" (baseUrl ++ collection)) decoder
--        |> Http.toTask


findAll : Rest item -> Task Error (List item)
findAll rest =
  case rest of
    Rest baseUrl collectionName decoder encoder ->
      HttpBuilder.get (baseUrl ++ collectionName)
      |> withExpect (Http.expectJson (Json.list decoder))
      |> toTask
--    get baseUrl (Json.list decoder) collection


post : String -> item -> Rest item -> Task Error String
post itemId item rest =
  case rest of
    Rest baseUrl collectionName decoder encoder ->
      HttpBuilder.post (baseUrl ++ collectionName)
      |> withExpect Http.expectString
      |> withHeader "Content-Type" "application/json"
      |> withJsonBody (encoder item)
      |> toTask
--    doSend "POST" [ ( "Content-Type", "application/json" ) ] baseUrl id body


postCommand : String -> Rest item -> Task Error String
postCommand command rest =
  case rest of
    Rest baseUrl collectionName decoder encoder ->
      HttpBuilder.post (baseUrl ++ collectionName ++ "/" ++ command)
      |> withExpect Http.expectString
      |> withHeader "Content-Type" "application/json"
      |> toTask


put : String -> item -> Rest item -> Task Error String
put itemId item rest =
  case rest of
    Rest baseUrl collectionName decoder encoder ->
      HttpBuilder.put (baseUrl ++ collectionName)
      |> withExpect Http.expectString
      |> withHeader "Content-Type" "application/json"
      |> withJsonBody (encoder item)
      |> toTask
--    doSend "PUT" [ ( "Content-Type", "application/json" ) ] baseUrl id body


delete : String -> Rest item -> Task Error String
delete itemId rest =
  case rest of
    Rest baseUrl collectionName decoder encoder ->
      HttpBuilder.delete (baseUrl ++ collectionName ++ "/" ++ itemId)
      |> withExpect Http.expectString
      |> toTask
--    doSend "DELETE" [] baseUrl url ""
