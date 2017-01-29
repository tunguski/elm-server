module AwaitingTable exposing (..)


import String
import Task exposing (..)
import Time exposing (second)
import Http exposing (Error(..))


import ApiPartApi exposing (..)
import BaseModel exposing (..)
import ExampleDb exposing (..)
import MongoDb exposing (..)
import Rest exposing (..)
import Server exposing (..)
import SessionModel exposing (Session)
import TichuModel exposing (..)
import TichuModelJson exposing (..)
import UserModel exposing (..)
import UrlParse exposing (..)


awaitingTablesApiPart :
    ApiPartApi msg
    -> Parse (Partial msg)
awaitingTablesApiPart api =
    P "awaitingTables"
        [ S (\id ->
            [ F
                (\() ->
                    case api.request.method of
                        Get ->
                            getAwaitingTable api id

                        Post ->
                            postAwaitingTable api id

                        _ ->
                            statusResponse 405 |> Result
                )
            , P "join"
                [ F (\() -> joinAwaitingTable api id)
                ]
            , P "start"
                [ F (\() -> startAwaitingTable api id)
                ]
            ]
          )
        , F (\() -> listAwaitingTables api)
        ]


{-| Start button pressed, awaiting for all players to confirm.

1. Load table
2. Check is actual player sitting at it
3. Mark player as wanting to play
4. If more players need to confirm, save change to db
5. If this is last confirmation, remove AwaitingTable and create Game

-}
startAwaitingTable api id =
    api.doWithSession
        (\session ->
            get id awaitingTables
            |> andThen (\table ->
                -- is player sitting at the table?
                case List.any (.name >> (==) session.username) table.users of
                    True ->
                        -- pressedStart & save
                        let
                            updatedTable =
                                { table
                                | users = List.map (\user ->
                                        if user.name == session.username
                                        then { user | pressedStart = True }
                                        else user
                                    ) table.users
                                }
                        in
                            put id updatedTable awaitingTables
                            |> andThen (createGame updatedTable)
                            |> andThenReturn (statusResponse 200 |> Task.succeed)
                    False ->
                        -- 404?
                        statusResponse 404 |> Task.succeed
            )
            |> onError logErrorAndReturn
        )


{-| Create Game if all players pressed start.
-}
createGame : AwaitingTable -> String -> Task Error String
createGame table r =
    -- if all players clicked start, create table based on awaiting table
    if List.all .pressedStart table.users
    then
        case List.map (\user -> get user.name users) table.users of
            a :: b :: c :: d :: [] ->
                put table.name
                    (initGame table.name table.test table.seed table.users)
                    games
                |> andThenReturn (succeed r)
            _ ->
                Debug.crash "Illegal state"
    else
        succeed r


{-| Join table awaiting for all players.

1. Load table
2. Check is actual player sitting at it
3. Add player if there is empty seat
4. Save change to db

-}
joinAwaitingTable api id =
    api.doWithSession
        (\session ->
            get id awaitingTables
            |> andThen (\table ->
                -- is player sitting at the table?
                case List.any (.name >> (==) session.username) table.users of
                    True ->
                        statusResponse 204 |> Task.succeed
                    False ->
                        put id { table | users =
                            (table.users
                             ++
                             [ AwaitingTableUser session.username api.request.time False ] )
                        } awaitingTables
                        |> andThenReturn (statusResponse 201 |> Task.succeed)
            )
            |> onError logErrorAndReturn
        )


{-| Return awaiting table with id.
-}
getAwaitingTable api id =
    api.doWithSession
        (\session ->
            get id awaitingTables
            |> andThen
                (\table ->
                    put id
                        { table
                        | users = List.map (\user ->
                                        if user.name /= session.username then
                                            user
                                        else
                                            { user | lastCheck = api.request.time }
                                    )
                                    table.users
                        } awaitingTables
                    |> andThenReturn
                        (table |> (encode awaitingTableEncoder >> okResponse >> Task.succeed))
                )
            |> onError logErrorAndReturn
        )

postAwaitingTable api id =
    api.doWithSession
        (\session ->
            (get id awaitingTables)
                -- create new table only if table was not found in db
                |>
                    onError
                        (\error ->
                            let
                                table =
                                    AwaitingTable id
                                        [ AwaitingTableUser session.username api.request.time False ]
                                        api.request.test
                                        (if api.request.test then
                                            getHeaderDefault "X-Test-Game-Seed"
                                                (String.toInt >> (\r ->
                                                    case r of
                                                        Ok i -> i
                                                        Err _ -> Basics.round api.request.time
                                                ))
                                                (Basics.round api.request.time)
                                                api.request
                                        else
                                            (Basics.round api.request.time))
                            in
                                put id table awaitingTables
                                    |> andThenReturn (Task.succeed table)
                        )
                -- if table exists, return error information that name is reserved
                -- return created table
                |>
                    andThen
                        (encode awaitingTableEncoder >> okResponse >> Task.succeed)
        )


userCheckTooOld api =
    .lastCheck >> (>) (api.request.time - 5 * second)


listAwaitingTables api =
    listDocuments awaitingTables
        |> andThen
            (.elements
             >> (\tables ->
                let
                    toUpdate =
                        List.filter
                            (.users >> List.any (userCheckTooOld api))
                            tables

                    updated =
                        List.map
                            (\table ->
                                { table
                                    | users =
                                        List.filter (userCheckTooOld api >> not) table.users
                                }
                            )
                            toUpdate

                    toRemove =
                        (List.filter (.users >> List.isEmpty) tables)
                            ++ (List.filter (.users >> List.isEmpty) updated)
                in
                    Task.sequence
                        ((List.map (\table -> delete table.name awaitingTables) toRemove)
                            ++ (updated
                                    |> List.filter (.users >> List.isEmpty >> not)
                                    |> List.map (\table -> put table.name table awaitingTables)
                               )
                        )
                        |> andThen
                            (\r ->
                                listDocuments awaitingTables
                                |> andThen (\tables ->
                                    succeed { tables
                                        -- filter test elements - we dont want to show it to players
                                        | elements = List.filter (.test >> (==) api.request.test) tables.elements
                                    }
                                )
                            )
            ))
        |> Task.attempt
            (\result ->
                case result of
                    Ok tables ->
                        api.sendResponse
                            (okResponse
                                (encodeCollection awaitingTableEncoder tables)
                            )
                    Err error ->
                      (error |> toString >> response 500 >> api.sendResponse)
            )
        |> Command


