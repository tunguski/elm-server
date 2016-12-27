module ApiPartApi exposing (..)


import Http exposing (Error)
import Task exposing (Task)


import Server exposing (..)
import SessionModel exposing (Session)


type alias ApiPartApi msg =
    { request : Request
    , doWithSession : ((Session -> Task Error Response) -> Partial msg)
    , sendResponse : (Response -> msg)
    }


