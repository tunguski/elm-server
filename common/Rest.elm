module Rest exposing (..)


import Http exposing (..)
import Json.Decode as Json
import Json.Encode as JE
import Platform.Cmd as Cmd
import Result exposing (Result(..))
import String exposing (concat)
import Task exposing (Task)


type alias RestMsg msg
  = Result Http.Error msg


type alias RestCollection item =
  { get : String -> Task Error item
  , post : String -> item -> Task Error String
  , postCommand : String -> Task Error String
  , put : String -> item -> Task Error String
  , delete : String -> Task Error String
  , all : Task Error (List item)
  --, find : String -> Task Error (Collection item)
  }


restCollection : String -> String -> Json.Decoder item -> (item -> JE.Value) -> RestCollection item
restCollection url name decoder encoder =
  let
    compositeUrl = url ++ name ++ "/"
    encode = encoder >> JE.encode 2
  in
    RestCollection 
      (get compositeUrl decoder)
      (\id item -> post compositeUrl id (encode item))
      (\id -> post compositeUrl id "")
      (\id item -> put compositeUrl id (encode item))
      (delete compositeUrl)
      (findAll url decoder name)


perform : (RestMsg value -> m) -> Task Error value -> Cmd m
perform msg task = 
  task
    |> Task.perform Err Ok
    |> Cmd.map msg


get : String -> (Json.Decoder item) -> String -> Task Error item
get baseUrl decoder collection =
  Http.get decoder <| Debug.log "url" (baseUrl ++ collection)


findAll : String -> (Json.Decoder item) -> String -> Task Error (List item)
findAll baseUrl decoder collection =
  get baseUrl (Json.list decoder) collection


post : String -> String -> String -> Task Error String
post baseUrl id body =
  doSend "POST" [ ("Content-Type", "application/json") ] baseUrl id body


put : String -> String -> String -> Task Error String
put baseUrl id body =
  doSend "PUT" [ ("Content-Type", "application/json") ] baseUrl id body


delete : String -> String -> Task Error String
delete baseUrl url =
  doSend "DELETE" [] baseUrl url ""


doSend : String -> List (String, String) -> String -> String -> String -> Task Error String
doSend method headers baseUrl url body =
   (Http.send defaultSettings
    { verb = method 
    , headers = headers 
    , url = baseUrl ++ url
    , body = Http.string body 
    })
    |> Task.mapError (\error ->
      case error of
        RawTimeout -> Timeout
        RawNetworkError -> NetworkError
    )
    |> Task.map (\response ->
      case response.value of
        Text body -> body
        _ -> ""
    )


