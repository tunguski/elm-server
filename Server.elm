port module Server exposing (..)

import Html exposing (..)
import Html.App as Html
import String
import Maybe 
import Dict exposing (Dict, empty)


type alias Initializer msg = Request -> (Response, List (Cmd msg))
type alias Updater msg = Request -> msg -> Response -> (Response, List (Cmd msg))


program : Initializer msg -> Updater msg -> Program Never
program initializer updater =
  Html.program
    { init = (empty, Cmd.none) 
    , view = view
    , update = update initializer updater
    , subscriptions = subscriptions
    }


-- MODEL


type alias Processing msg =
  { request : Request
  , response : Response
  , queue : List (Cmd msg)
  }


type alias Model msg = Dict String (Processing msg)


-- UPDATE


type Msg msg
  = IncomingRequest Request 
  | InternalMsg String msg


port sendResponse : Response -> Cmd msg


update : Initializer m 
          -> Updater m 
          -> Msg m 
          -> Model m 
          -> (Model m, Cmd (Msg m))
update initializer updater msg model =
  case msg of
    IncomingRequest request ->
      let
        (response, cmds) = initializer request
      in
        if List.isEmpty cmds
        then
          model
          ! [ sendResponse response ] 
        else
          Dict.insert request.id 
            (Processing request response cmds)
            model
          -- FIXME: map cmds to Server.Msg
          ! (List.map (\s -> Cmd.map (InternalMsg request.id) s) cmds)
    InternalMsg text internalMessage ->
      case Dict.get text model of
        Just processing ->
          let
            (response, cmds) = updater processing.request internalMessage processing.response 
          in
            if List.isEmpty cmds
            then
              Dict.remove processing.request.id model
              ! [ sendResponse response ] 
            else
              Dict.insert processing.request.id 
                (Processing processing.request response cmds)
                model
              -- FIXME: map cmds to Server.Msg
              ! (List.map (\s -> Cmd.map (InternalMsg processing.request.id) s) cmds)
        Nothing ->
          model ! []



-- SUBSCRIPTIONS


type alias Request =
  { id : String
  , url : String
  , method : String
  }


type alias Response =
  { idRequest : String
  , statusCode : Int
  , body : String
  }


port request : (Request -> msg) -> Sub msg


subscriptions : Model msg -> Sub (Msg msg)
subscriptions model =
  request IncomingRequest 


-- VIEW

view : Model msg -> Html (Msg msg)
view model = text "Use the API Luke!"


