port module Server exposing (..)

import Html exposing (..)
import Html.App as Html
import String
import Maybe
import Dict exposing (Dict, empty)


-- generic state of request processing
type alias State msg state = ((Response, state), List (Cmd msg))
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


initResponseStatus idRequest statusCode =
  Response idRequest [("Server", "Tunguski's Elm-Server")] statusCode ""


initResponse idRequest =
  initResponseStatus idRequest 200


type alias Processing msg state =
  { request : Request
  , response : (Response, state)
  , queue : List (Cmd msg)
  }


type alias Model msg state 
  = Dict String (Processing msg state)


-- UPDATE


type Msg msg
  = IncomingRequest Request
  | InternalMsg String msg


port sendResponse : Response -> Cmd msg


update : Initializer m state
          -> Updater m state
          -> Msg m
          -> Model m state
          -> (Model m state, Cmd (Msg m))
update initializer updater msg model =
  case msg of
    IncomingRequest request ->
      let
        ((response, st), cmds) = initializer request
      in
        if List.isEmpty cmds
        then
          model
          ! [ sendResponse response ]
        else
          Dict.insert request.id
            (Processing request (response, st) cmds)
            model
          ! (List.map (\s -> Cmd.map (InternalMsg request.id) s) cmds)
    InternalMsg idRequest internalMessage ->
      case Dict.get idRequest model of
        Just processing ->
          let
            ((response, st), cmds) =
             updater processing.request internalMessage processing.response
          in
            if List.isEmpty cmds
            then
              Dict.remove processing.request.id model
              ! [ sendResponse response ]
            else
              Dict.insert processing.request.id
                (Processing processing.request (response, st) cmds)
                model
              ! (List.map (\s -> Cmd.map (InternalMsg processing.request.id) s) cmds)
        Nothing ->
          model ! []


port request : (Request -> msg) -> Sub msg


-- SUBSCRIPTIONS


subscriptions : Model msg state -> Sub (Msg msg)
subscriptions model =
  request IncomingRequest


-- VIEW

view : Model msg state -> Html (Msg msg)
view model = text "Use the API Luke!"


