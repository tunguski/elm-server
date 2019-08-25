port module Server exposing (Initializer, Model, Partial(..), PortRequest, PortResponse, PostRequestUpdater, Request, RequestMethod(..), Response, ServerMsg(..), Updater, baseRequest, containsParam, executeIfIdSessionExists, fakeResponse, getCookies, getHeader, getHeaderDefault, getIdSession, getParam, logErrorAndReturn, okResponse, parseRequest, parseRequestMethod, program, request, response, sendResponse, sendResponsePort, setCookie, statusResponse, subscriptions, update, x_test_session)

import Dict exposing (Dict, empty)
import Http exposing (Error(..))
import Maybe
import Platform exposing (worker)
import String
import Task
import Time


type Partial msg
    = Result Response
    | Command (Cmd msg)
    | Noop



-- -- generic state of request processing
-- type alias State msg state = ((Response, state), Maybe (Cmd msg))
-- main loop functions


type alias Initializer msg =
    Request -> Partial msg


type alias Updater msg =
    Request -> msg -> Partial msg


type alias PostRequestUpdater msg =
    Request -> Maybe (Cmd msg)



-- -- helper function that modifies state according to message
-- type alias StateUpdater msg state = State msg state -> State msg state


program :
    Initializer msg
    -> Updater msg
    -> PostRequestUpdater msg
    -> Program () (Dict String Request) (ServerMsg msg)
program initializer updater postRequestUpdater =
    Platform.worker
        { init = \() -> ( empty, Cmd.none )
        , update = update initializer updater postRequestUpdater
        , subscriptions = subscriptions
        }



-- MODEL


type RequestMethod
    = Get
    | Post
    | Put
    | Patch
    | Delete


type alias Request =
    { id : String
    , time : Int
    , headers : List ( String, String )
    , queryParams : List ( String, String )
    , url : String
    , method : RequestMethod
    , body : String
    , idSession : Maybe String
    , test : Bool
    }


type alias PortRequest =
    { id : String
    , time : Int
    , headers : List ( String, String )
    , queryParams : List ( String, String )
    , url : String
    , method : String
    , body : String
    }


baseRequest r =
    Request
        r.id
        r.time
        r.headers
        r.queryParams
        r.url
        Get
        r.body
        Nothing
        False


parseRequest : PortRequest -> Maybe Request
parseRequest r =
    case parseRequestMethod r.method of
        Just method ->
            let
                request_ =
                    baseRequest r
            in
            Just
                { request_
                    | method = method
                    , idSession = getIdSession request_
                    , test =
                        getHeaderDefault "X-Test-Session"
                            (\_ -> True)
                            False
                            request_
                }

        Nothing ->
            Nothing


type alias Response =
    { headers : List ( String, String )
    , statusCode : Int
    , body : String
    }


type alias PortResponse =
    { idRequest : String
    , headers : List ( String, String )
    , statusCode : Int
    , body : String
    }


okResponse : String -> Response
okResponse body =
    Response [] 200 body


statusResponse : Int -> Response
statusResponse status =
    Response [] status ""


response : Int -> String -> Response
response status body =
    Response [] status body


logErrorAndReturn error =
    case Debug.log "error" error of
        BadStatus r ->
            statusResponse r.status.code |> Task.succeed

        _ ->
            statusResponse 500 |> Task.succeed


parseRequestMethod : String -> Maybe RequestMethod
parseRequestMethod method =
    case String.toLower method of
        "get" ->
            Just Get

        "post" ->
            Just Post

        "put" ->
            Just Put

        "patch" ->
            Just Patch

        "delete" ->
            Just Delete

        _ ->
            Nothing


getCookies : Request -> Dict String String
getCookies request_ =
    let
        extractCookie cookie =
            case String.split "=" <| String.trim cookie of
                key :: value :: t ->
                    Just ( key, value )

                _ ->
                    Nothing

        splitCookies cookie =
            String.split ";" cookie
    in
    case getHeader "cookie" request_ of
        Just cookies ->
            splitCookies cookies
                |> List.map extractCookie
                |> List.filterMap identity
                |> Dict.fromList

        Nothing ->
            Dict.empty


x_test_session =
    "X-Test-Session"


getIdSession : Request -> Maybe String
getIdSession request_ =
    case getHeader x_test_session request_ of
        Just id ->
            Just id

        Nothing ->
            Dict.get "SESSIONID" <| getCookies request_


fakeResponse : Int -> Http.Response String
fakeResponse status =
    Http.Response "" { code = status, message = "" } Dict.empty ""


executeIfIdSessionExists request_ task =
    case request_.idSession of
        Just id ->
            task id

        Nothing ->
            Task.fail (BadStatus (fakeResponse 404))


getHeaderDefault : String -> (String -> a) -> a -> Request -> a
getHeaderDefault key mapper default request_ =
    getHeader key request_
        |> Maybe.map mapper
        |> Maybe.withDefault default


getHeader : String -> Request -> Maybe String
getHeader key request_ =
    List.filter (\( headerKey, value ) -> headerKey == String.toLower key) request_.headers
        |> List.head
        |> Maybe.map (\( k, v ) -> v)


setCookie : String -> String -> Response -> Response
setCookie name value response_ =
    { response_ | headers = ( name, value ) :: response_.headers }


containsParam : String -> Request -> Bool
containsParam name request_ =
    List.any (\( key, value ) -> key == name) request_.queryParams


getParam : String -> Request -> Maybe String
getParam name request_ =
    List.filter (\( key, value ) -> key == name) request_.queryParams
        |> List.head
        |> Maybe.map (\( key, value ) -> value)


type alias Model =
    Dict String Request



-- UPDATE


type ServerMsg msg
    = IncomingRequest PortRequest
    | InternalServerMsg String msg
    | InternalPostRequestMsg String msg


port sendResponsePort : PortResponse -> Cmd msg


sendResponse : Request -> Response -> Cmd msg
sendResponse request_ response_ =
    PortResponse request_.id response_.headers response_.statusCode response_.body
        |> sendResponsePort


update :
    Initializer m
    -> Updater m
    -> PostRequestUpdater m
    -> ServerMsg m
    -> Model
    -> ( Model, Cmd (ServerMsg m) )
update initializer updater postRequestUpdater msg model =
    case msg of
        IncomingRequest r ->
            case parseRequest r of
                Just request_ ->
                    case initializer request_ of
                        Result response_ ->
                            case postRequestUpdater request_ of
                                Just msg_ ->
                                    ( Dict.insert request_.id request_ model
                                    , Cmd.batch
                                        [ sendResponse request_ response_
                                        , Cmd.map (InternalPostRequestMsg request_.id) msg_
                                        ]
                                    )

                                Nothing ->
                                    ( model
                                    , sendResponse request_ response_
                                    )

                        Command cmd ->
                            ( Dict.insert request_.id request_ model
                            , Cmd.map (InternalServerMsg request_.id) cmd
                            )

                        Noop ->
                            ( model
                            , Cmd.none
                            )

                Nothing ->
                    ( model
                    , sendResponse (baseRequest r) (Response [] 400 "")
                    )

        InternalServerMsg idRequest internalMessage ->
            case Dict.get idRequest model of
                Just request_ ->
                    case updater request_ internalMessage of
                        Result response_ ->
                            case postRequestUpdater request_ of
                                Just msg_ ->
                                    ( model
                                    , Cmd.batch
                                        [ sendResponse request_ response_
                                        , Cmd.map (InternalPostRequestMsg request_.id) msg_
                                        ]
                                    )

                                Nothing ->
                                    ( Dict.remove request_.id model
                                    , sendResponse request_ response_
                                    )

                        Command cmd ->
                            ( model
                            , Cmd.map (InternalServerMsg idRequest) cmd
                            )

                        Noop ->
                            ( model
                            , Cmd.none
                            )

                Nothing ->
                    Debug.log "Illegal state" <|
                        ( model
                        , Cmd.none
                        )

        InternalPostRequestMsg idRequest postRequestMsg ->
            case Dict.get idRequest model of
                Just request_ ->
                    case updater request_ postRequestMsg of
                        Result response_ ->
                            Debug.log "Illegal state" <|
                                ( model
                                , Cmd.none
                                )

                        Command cmd ->
                            ( model
                            , Cmd.map (InternalServerMsg idRequest) cmd
                            )

                        Noop ->
                            ( Dict.remove idRequest model
                            , Cmd.none
                            )

                Nothing ->
                    Debug.log "Illegal state" <|
                        ( model
                        , Cmd.none
                        )


port request : (PortRequest -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub (ServerMsg msg)
subscriptions model =
    request IncomingRequest
