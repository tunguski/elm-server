module AwaitingTable exposing (..)


import String
import Task exposing (Task)
import Time exposing (second)
import Http exposing (Error (BadResponse))


import BaseModel exposing (..)
import ExampleDb exposing (..) 
import Server exposing (..)
import SessionModel exposing (Session)
import UserModel exposing (..)
import UrlParse exposing (..)
import TichuModel exposing (..)
import TichuModelJson exposing (..)


awaitingTablesApiPart : Request
    -> ((Session -> Task Error Response) -> Partial msg)
    -> (Response -> msg)
    -> Parse (Partial msg)
awaitingTablesApiPart request doWithSession sendResponse =
  P "awaitingTables" 
    [ S (\id -> 
          [ F (\() -> 
                case parseRequestMethod request.method of
                  Get ->
                    doWithSession (\session ->
                      awaitingTables.get id
                        |> andThen 
                            (\table ->
                              awaitingTables.put id { table | users =
                                List.map (\user ->
                                  case user of
                                    (name, time) ->
                                      if name /= session.username then user else (name, request.time)
                                ) table.users
                              }
                              |> andThenReturn
                                  (table |> (encode awaitingTableEncoder >> okResponse >> Task.succeed))
                            )
                        |> onError (\error -> 
                            let
                              x = Debug.log "error" error
                            in
                              statusResponse 404 |> Task.succeed
                        )
                    )
                  Post ->
                    doWithSession (\session ->
                      (awaitingTables.get id)
                        -- create new table only if table was not found in db
                        |> onError (\error ->
                          let
                            table = 
                              AwaitingTable id 
                                [ (session.username, request.time) ]
                          in
                            awaitingTables.put id table
                            |> andThenReturn (Task.succeed table)
                        )
                        -- if table exists, return error information that name is reserved
    
                        -- return created table
                        |> andThen 
                            (encode awaitingTableEncoder >> okResponse >> Task.succeed)
                    )
                  _ ->
                    statusResponse 405
                      |> Result
              )
          , P "join"
              [ F (\() -> 
                    Result (okResponse ("[TODO] Joined: " ++ id))
                  )
              ]
          ]
        )
    , F (\() ->
          awaitingTables.all
            |> andThen (\tables ->
                let
                  toUpdate = 
                    List.filter 
                      (.users >> List.any (\(name, time) -> (time + (5 * second) < request.time))) 
                      tables.elements
                  updated =
                    List.map 
                      (\table -> 
                        { table | users = 
                            List.filter (\(name, time) -> (time + (5 * second) > request.time)) table.users })
                      toUpdate
                  toRemove = 
                    (List.filter (.users >> List.isEmpty) tables.elements)
                    ++
                    (List.filter (.users >> List.isEmpty) updated)
                in
                  Task.sequence 
                    ((List.map (\table -> awaitingTables.delete table.name) toRemove)
                     ++
                     (updated
                      |> List.filter (.users >> List.isEmpty >> not)
                      |> List.map (\table -> awaitingTables.put table.name table)
                     )
                    )
                    |> andThen (\r ->
                      awaitingTables.all
                    )
            )
            |> Task.perform
                 (toString >> response 500 >> sendResponse)
                 (\tables -> sendResponse (okResponse 
                   (encodeCollection awaitingTableEncoder tables)))
            |> Command
        )
    ]


