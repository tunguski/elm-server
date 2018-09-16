module ApiPartApi exposing (ApiPartApi)

import Http exposing (Error)
import Server exposing (..)
import SessionModel exposing (Session)
import Task exposing (Task)


type alias ApiPartApi msg =
    { request : Request
    , doWithSession : (Session -> Task Error Response) -> Partial msg
    , sendResponse : Response -> msg
    }
