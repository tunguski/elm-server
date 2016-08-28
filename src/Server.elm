port module Server exposing (..)

import Html exposing (..)
import Html.App as Html
import String
import Maybe
import Dict exposing (Dict, empty)


-- generic state of request processing
type alias State msg state = ((Response, state), Maybe (Cmd msg))
-- main loop functions
type alias Initializer msg state = Request -> State msg state 
type alias Updater msg state = Request -> msg -> (Response, state) -> State msg state 
-- helper function that modifies state according to message
type alias StateUpdater msg state = State msg state -> State msg state 


program : Initializer msg state -> Updater msg state -> Program Never
program initializer updater =
  Html.program
    { init = (empty, Cmd.none)
    , view = view
    , update = update initializer updater
    , subscriptions = subscriptions
    }


-- MODEL


type alias Request =
  { id : String
  , headers : List (String, String) 
  , url : String
  , method : String
  }


type alias Response =
  { idRequest : String
  , headers : List (String, String) 
  , statusCode : Int
  , body : String
  }


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


getHeader : String -> Request -> Maybe String
getHeader key request =
  List.filter (\((headerKey, value)) -> headerKey == key) request.headers
    |> List.head
    |> Maybe.map (\((k, v)) -> v)
    


initResponseStatus idRequest statusCode =
  Response idRequest [("Server", "Tunguski's Elm-Server")] statusCode ""


initResponse idRequest =
  initResponseStatus idRequest 200


type alias Processing state =
  { request : Request
  , response : (Response, state)
  }


type alias Model state 
  = Dict String (Processing state)


-- UPDATE


type Msg msg
  = IncomingRequest Request
  | InternalMsg String msg


port sendResponse : Response -> Cmd msg


update : Initializer m state
          -> Updater m state
          -> Msg m
          -> Model state
          -> (Model state, Cmd (Msg m))
update initializer updater msg model =
  case msg of
    IncomingRequest request ->
      let
        ((response, st), cmd) = initializer request
      in
        case cmd of
          Just innerMsg ->
            Dict.insert request.id
              (Processing request (response, st))
              model
            ! [Cmd.map (InternalMsg request.id) innerMsg]
          Nothing ->
            model
            ! [ sendResponse response ]
    InternalMsg idRequest internalMessage ->
      case Dict.get idRequest model of
        Just processing ->
          let
            ((response, st), cmd) =
               updater processing.request internalMessage processing.response
          in
            case cmd of
              Just innerMsg ->
                Dict.insert processing.request.id
                  (Processing processing.request (response, st))
                  model
                ! [Cmd.map (InternalMsg processing.request.id) innerMsg]
              Nothing ->
                Dict.remove processing.request.id model
                ! [ sendResponse response ]
        Nothing ->
          model ! []


port request : (Request -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model state -> Sub (Msg msg)
subscriptions model =
  request IncomingRequest


-- VIEW

view : Model state -> Html (Msg msg)
view model = text "Use the API Luke!"


