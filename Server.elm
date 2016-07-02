port module Server exposing (..)

import Html exposing (..)
import Html.App as Html
import String
import Dict exposing (Dict, empty)


main =
  Html.program
    { init = (Model empty, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Processing =
  { request : Request
  , response : Response
  , queue : List (Cmd Msg)
  }


type alias Model =
  { processing : Dict String Processing
  }


-- UPDATE


type Msg
  = IncomingRequest Request 


port sendResponse : Response -> Cmd msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    IncomingRequest request ->
     { model |
        processing = Dict.insert request.id 
          (Processing request (Response request.id 200 "") [])
          model.processing
      } ! [ sendResponse 
            <| Response request.id 200 
            <| String.concat [ "Request ", request.url ] ]


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


subscriptions : Model -> Sub Msg
subscriptions model =
  request IncomingRequest 


-- VIEW

view : Model -> Html Msg
view model = text "Use the API Luke!"


