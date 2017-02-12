port module Server exposing (..)

import Dict exposing (Dict, empty)
import Maybe
import Http exposing (Error(..))
import Platform exposing (program)
import String
import Task
import Time exposing (Time)


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


program : Initializer msg ->
          Updater msg ->
          PostRequestUpdater msg ->
          Program Never (Dict String Request) (ServerMsg msg)
program initializer updater postRequestUpdater =
    Platform.program
        { init = ( empty, Cmd.none )
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
    , time : Time
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
    , time : Time
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
                request = baseRequest r
            in
                Just { request
                     | method = method
                     , idSession = getIdSession request
                     , test = getHeaderDefault "X-Test-Session"
                                (\_ -> True) False request
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
        "get" ->    Just Get
        "post" ->   Just Post
        "put" ->    Just Put
        "patch" ->  Just Patch
        "delete" -> Just Delete
        _ -> Nothing


getCookies : Request -> Dict String String
getCookies request =
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
        case getHeader "cookie" request of
            Just cookies ->
                splitCookies cookies
                    |> List.map extractCookie
                    |> List.filterMap identity
                    |> Dict.fromList

            Nothing ->
                Dict.empty


x_test_session = "X-Test-Session"


getIdSession : Request -> Maybe String
getIdSession request =
    case getHeader x_test_session request of
        Just id ->
            Just id
        Nothing ->
            Dict.get "SESSIONID" <| getCookies request


fakeResponse : Int -> Http.Response String
fakeResponse status =
    Http.Response "" { code = status, message = "" } Dict.empty ""


executeIfIdSessionExists request task =
    case request.idSession of
        Just id ->
            task id
        Nothing ->
            Task.fail (BadStatus (fakeResponse 404))


getHeaderDefault : String -> (String -> a) -> a -> Request -> a
getHeaderDefault key mapper default request =
    getHeader key request
    |> Maybe.map mapper
    |> Maybe.withDefault default


getHeader : String -> Request -> Maybe String
getHeader key request =
    List.filter (\( headerKey, value ) -> headerKey == (String.toLower key)) request.headers
        |> List.head
        |> Maybe.map (\( k, v ) -> v)


setCookie : String -> String -> Response -> Response
setCookie name value response =
    { response | headers = ( name, value ) :: response.headers }


containsParam : String -> Request -> Bool
containsParam name request =
    List.any (\(key, value) -> key == name) request.queryParams


type alias Model =
    Dict String Request


-- UPDATE


type ServerMsg msg
    = IncomingRequest PortRequest
    | InternalServerMsg String msg
    | InternalPostRequestMsg String msg


port sendResponsePort : PortResponse -> Cmd msg


sendResponse : Request -> Response -> Cmd msg
sendResponse request response =
    PortResponse request.id response.headers response.statusCode response.body
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
                Just request ->
                    case initializer request of
                        Result response ->
                            case postRequestUpdater request of
                                Just msg ->
                                    Dict.insert request.id request model !
                                        [ sendResponse request response
                                        , Cmd.map (InternalPostRequestMsg request.id) msg
                                        ]
                                Nothing ->
                                    model ! [ sendResponse request response ]

                        Command cmd ->
                            Dict.insert request.id request model
                                ! [ Cmd.map (InternalServerMsg request.id) cmd ]
                        Noop ->
                            model ! []

                Nothing ->
                    model ! [ sendResponse (baseRequest r) (Response [] 400 "") ]

        InternalServerMsg idRequest internalMessage ->
            case Dict.get idRequest model of
                Just request ->
                    case updater request internalMessage of
                        Result response ->
                            case postRequestUpdater request of
                                Just msg ->
                                    model !
                                        [ sendResponse request response
                                        , Cmd.map (InternalPostRequestMsg request.id) msg
                                        ]
                                Nothing ->
                                    Dict.remove request.id model ! [ sendResponse request response ]

                        Command cmd ->
                            model
                                ! [ Cmd.map (InternalServerMsg idRequest) cmd ]

                        Noop ->
                            model ! []

                Nothing ->
                    Debug.log "Illegal state" <| model ! []

        InternalPostRequestMsg idRequest postRequestMsg ->
            Dict.remove idRequest model ! []


port request : (PortRequest -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub (ServerMsg msg)
subscriptions model =
    request IncomingRequest


