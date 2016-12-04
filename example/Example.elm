module Example exposing (..)


import Base64
import Debug
import Json.Decode as JD
import Dict
import Http exposing (Error(..))
import Json.Decode as Json exposing (..)
import RandomTask exposing (..)
import String
import Result exposing (Result)
import Task exposing (Task)


import BaseModel exposing (..)
import MongoDb exposing (DbMsg(..), MongoDb)
import Server exposing (..)
import Session exposing (..)
import SessionModel exposing (..)
import Table exposing (..)
import Repo exposing (..)
import UrlParse exposing (..)
import ExampleDb exposing (..)


main =
  Server.program init update


-- UPDATE


type Msg 
  = SendResponse Response


withSession : Request -> (Session -> Task Error Response) -> Partial Msg
withSession request action =
  getSession (getIdSession request)
    |> processTask action


withSessionMaybe : Request -> (Error -> Msg) -> (Session -> Task Error Response) -> Partial Msg
withSessionMaybe request errorProcessor action =
  getSession (getIdSession request)
    |> processTaskWithError errorProcessor action


processTaskWithError : (x -> Msg) -> (item -> Task x Response) -> Task x item -> Partial Msg
processTaskWithError errorProcessor eval task =
  Task.andThen task eval
    |> Task.perform
         errorProcessor
         SendResponse
    |> Command


processTask : (item -> Task x Response) -> Task x item -> Partial Msg
processTask eval task =
  processTaskWithError
    (\error ->
      let
        debug = Debug.log "error" error
      in
        SendResponse (statusResponse 500))
    eval
    task


-- init : Request -> Partial Msg
init : Initializer Msg
init request =
    let
      doWithSession = withSession request
      -- serialize provided object and return it as successful task response with status 200
      restMap =
        P "api"
          [ sessionApiPart request doWithSession withSessionMaybe SendResponse
          , tablesApiPart request doWithSession SendResponse
          , F (\() -> 
                  getRepoInfos 
                    |> processTask (toString >> okResponse >> Task.succeed)
              )
          ]
    in
      case parse restMap request.url of
        Ok response ->
          response
        Err error ->
          Result (statusResponse 404) 


-- update : Request -> Msg -> Partial Msg
update : Updater Msg
update request msg =
  case Debug.log "update msg" msg of
    SendResponse response -> 
      Result response


