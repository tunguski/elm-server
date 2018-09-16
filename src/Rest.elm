module Rest exposing
    ( Rest
    , RestConfig
    , RestResult
    , andThenReturn
    , config
    , delete
    , findAll
    , get
    , ignoreError
    , post
    , postCommand
    , put
    , restCollection
    , withBody
    , withHeader
    , withQueryParams
    )

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


type Rest item
    = Rest (RestConfig item) (List (RequestBuilder item -> RequestBuilder item))


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
        Rest config_ modifiers ->
            config_


ignoreError : Task y a -> Task x a -> Task y a
ignoreError errorTask task =
    Task.onError (\error -> errorTask) task


andThenReturn : Task x b -> Task x a -> Task x b
andThenReturn fn task =
    Task.andThen (\result -> fn) task


withBody : String -> Rest item -> Rest item
withBody body rest =
    case rest of
        Rest config_ modifiers ->
            Rest config_ (modifiers ++ [ HttpBuilder.withStringBody "application/json" body ])


withHeader : String -> String -> Rest item -> Rest item
withHeader key value rest =
    case rest of
        Rest config_ modifiers ->
            Rest config_ (modifiers ++ [ HttpBuilder.withHeader key value ])


withQueryParams : List ( String, String ) -> Rest item -> Rest item
withQueryParams params rest =
    case rest of
        Rest config_ modifiers ->
            Rest config_ (modifiers ++ [ HttpBuilder.withQueryParams params ])


apply : List (RequestBuilder item -> RequestBuilder item) -> RequestBuilder item -> RequestBuilder item
apply modifiers builder =
    -- fold all modifiers
    List.foldl
        (\modifier b ->
            modifier b
        )
        builder
        modifiers


baseQuery : Rest item -> (RestConfig item -> RequestBuilder item -> RequestBuilder a) -> Task Error a
baseQuery rest custom =
    case rest of
        Rest config_ modifiers ->
            HttpBuilder.get (config_.baseUrl ++ config_.collectionName)
                |> withExpect (Http.expectJson config_.decoder)
                |> apply modifiers
                |> custom config_
                |> toTask


withMethod method builder =
    { builder | method = method }


subResource name builder =
    { builder | url = builder.url ++ "/" ++ name }


logError method =
    Task.mapError
        (\error ->
            let
                log =
                    Debug.log ("Error during " ++ method) error
            in
            error
        )


get : String -> Rest item -> Task Error item
get itemId rest =
    baseQuery rest (\_ -> subResource itemId)
        |> logError "GET"


findAll : Rest item -> Task Error (List item)
findAll rest =
    baseQuery rest
        (withExpect << Http.expectJson << Json.list << .decoder)


post : String -> item -> Rest item -> Task Error String
post itemId item rest =
    baseQuery rest
        (\config_ ->
            subResource itemId
                >> withMethod "post"
                >> withJsonBody (config_.encoder item)
                >> HttpBuilder.withHeader "Content-Type" "application/json"
                >> withExpect Http.expectString
        )


postCommand : String -> Rest item -> Task Error String
postCommand command rest =
    baseQuery rest
        (\_ ->
            subResource command
                >> withMethod "post"
                >> HttpBuilder.withHeader "Content-Type" "application/json"
                >> withExpect Http.expectString
        )


put : String -> item -> Rest item -> Task Error String
put itemId item rest =
    baseQuery rest
        (\config_ ->
            subResource itemId
                >> withMethod "put"
                >> withJsonBody (config_.encoder item)
                >> HttpBuilder.withHeader "Content-Type" "application/json"
                >> withExpect Http.expectString
        )
        |> logError "PUT"


delete : String -> Rest item -> Task Error String
delete itemId rest =
    baseQuery rest
        (\_ ->
            subResource itemId
                >> withMethod "delete"
                >> withExpect Http.expectString
        )
