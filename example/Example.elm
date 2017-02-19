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


import ApiPartApi exposing (..)
import AwaitingTable exposing (..)
import Game exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import MongoDb exposing (..)
import Repo exposing (..)
import Rest exposing (..)
import Server exposing (..)
import Session exposing (..)
import SessionModel exposing (..)
import Table exposing (..)
import UrlParse exposing (..)


main =
    Server.program init update postRequestUpdater


-- UPDATE


type Msg
    = SendResponse Response
    | PostRequestMsg (Result String (Maybe (Cmd Msg)))


withSession : Request -> (Session -> Task Error Response) -> Partial Msg
withSession request action =
    executeIfIdSessionExists request (\id -> get id sessions)
    |> processTask action


withSessionMaybe : Request -> (Error -> Msg) -> (Session -> Task Error Response) -> Partial Msg
withSessionMaybe request errorProcessor action =
    executeIfIdSessionExists request (\id -> get id sessions)
    |> processTaskWithError errorProcessor action


processTaskWithError : (x -> Msg) -> (item -> Task x Response) -> Task x item -> Partial Msg
processTaskWithError errorProcessor eval task =
    Task.andThen eval task
        |> Task.attempt
            (\result ->
                case result of
                    Ok response ->
                        SendResponse response
                    Err error ->
                        errorProcessor error
            )
        |> Command


processTask : (item -> Task x Response) -> Task x item -> Partial Msg
processTask eval task =
    processTaskWithError
        (\error ->
            let
                debug =
                    Debug.log "error" error
            in
                SendResponse (statusResponse 500)
        )
        eval
        task


requiredTables =
    [ collectionUrl sessions
    , collectionUrl users
    , collectionUrl games
    , collectionUrl awaitingTables
    --, collectionUrl archiveGames
    ]


performBatch tasks =
    Task.sequence tasks
        |> Task.attempt
            (\result ->
                case result of
                    Ok data ->
                        data |> (toString >> okResponse >> SendResponse)
                    Err error ->
                        error |> (toString >> response 500 >> SendResponse)
            )
        |> Command


initDb : () -> Partial Msg
initDb _ =
   performBatch
        (List.map createCollection requiredTables)


eraseDb : () -> Partial Msg
eraseDb _ =
    performBatch <|
        (List.map deleteCollection requiredTables)
        ++ (List.map createCollection requiredTables)


init : Initializer Msg -- Request -> Partial Msg
init request =
    let
        api = ApiPartApi request (withSession request) SendResponse

        -- serialize provided object and return it as successful task response with status 200
        restMap =
            P "api"
                [ sessionApiPart api withSessionMaybe
                , tablesApiPart api
                , awaitingTablesApiPart api
                , gamesApiPart api
                , P "init"
                    [ F initDb ]
                , P "eraseDb"
                    [ F eraseDb ]
                ]
    in
        case parse restMap request.url of
            Ok response ->
                response

            Err error ->
                Result (statusResponse 404)


update : Updater Msg -- Request -> Msg -> Partial Msg
update request msg =
    case msg of
        SendResponse response ->
            Result response

        PostRequestMsg result ->
            case Debug.log "PRM result" result of
                Ok (Just cmd) ->
                    Command cmd
                _ ->
                    Noop


postRequestUpdater request =
    let
        restMap =
            P "api"
                [ gamePostRequestPart PostRequestMsg request
                ]
    in
        case Debug.log "pRU" <| parse restMap request.url of
            Ok cmd -> cmd
            Err error -> Nothing


