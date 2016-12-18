port module Server exposing (..)

import Html exposing (..)
import Html.App as Html
import String
import Maybe
import Task
import Time exposing (Time)
import Dict exposing (Dict, empty)


type Partial msg
  = Result Response
  | Command (Cmd msg)

-- -- generic state of request processing
-- type alias State msg state = ((Response, state), Maybe (Cmd msg))
-- main loop functions
type alias Initializer msg = Request -> Partial msg
type alias Updater msg = Request -> msg -> Partial msg
-- -- helper function that modifies state according to message
-- type alias StateUpdater msg state = State msg state -> State msg state 


program : Initializer msg -> Updater msg -> Program Never
program initializer updater =
  Html.program
    { init = (empty, Cmd.none)
    , view = view
    , update = update initializer updater
    , subscriptions = subscriptions
    }


-- MODEL


type RequestMethod
  = Get
  | Post
  | Put
  | Patch
  | Delete
  | MalformedMethod


type alias Request =
  { id : String
  , time : Time
  , headers : List (String, String) 
  , url : String
  , method : String
  , body : String
  }


type alias Response =
  { headers : List (String, String) 
  , statusCode : Int
  , body : String
  }


type alias PortResponse =
  { idRequest : String
  , headers : List (String, String) 
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


parseRequestMethod : String -> RequestMethod
parseRequestMethod method =
  case String.toLower method of
    "get" -> Get
    "post" -> Post
    "put" -> Put
    "patch" -> Patch
    "delete" -> Delete
    _ -> MalformedMethod



getCookies : Request -> Dict String String
getCookies request =
  let
    extractCookie cookie =
      case String.split "=" <| String.trim cookie of
        key :: value :: t ->
          Just (key, value)
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
      Nothing -> Dict.empty


getIdSession : Request -> String
getIdSession request =
  case Dict.get "SESSIONID" <| getCookies request of
    Just id -> id
    Nothing -> "empty"


getHeader : String -> Request -> Maybe String
getHeader key request =
  List.filter (\((headerKey, value)) -> headerKey == key) request.headers
    |> List.head
    |> Maybe.map (\((k, v)) -> v)
  

setCookie : String -> String -> Response -> Response
setCookie name value response =
  { response | headers = (name, value) :: response.headers }


type alias Model
  = Dict String Request


-- UPDATE


type ServerMsg msg
  = IncomingRequest Request
  | InternalServerMsg String msg


port sendResponsePort : PortResponse -> Cmd msg

sendResponse : Request -> Response -> Cmd msg
sendResponse request response =
  PortResponse request.id response.headers response.statusCode response.body
    |> sendResponsePort


update : Initializer m
      -> Updater m
      -> ServerMsg m
      -> Model
      -> (Model, Cmd (ServerMsg m))
update initializer updater msg model =
  case msg of
    IncomingRequest request ->
      case initializer request of
        Result response ->
          model ! [ sendResponse request response ]
        Command cmd ->
          Dict.insert request.id request model
            ! [ Cmd.map (InternalServerMsg request.id) cmd ]
    InternalServerMsg idRequest internalMessage ->
      case Dict.get idRequest model of
        Just request ->
          case updater request internalMessage of
            Result response ->
              model ! [ sendResponse request response ]
            Command cmd ->
              Dict.remove request.id model
                ! [ Cmd.map (InternalServerMsg idRequest) cmd ]
        Nothing ->
          Debug.crash "Illegal state"


port request : (Request -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model -> Sub (ServerMsg msg)
subscriptions model =
  request IncomingRequest


-- VIEW

view : Model -> Html (ServerMsg msg)
view model = text "Use the API Luke!"


