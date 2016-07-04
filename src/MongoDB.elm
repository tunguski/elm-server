module MongoDB exposing (..)


import String exposing (concat)


type alias RestBackend =
  { get     : Decoder value -> String -> Task Error value
  , post    : Decoder value -> String -> Body -> Task Error value
  , put     : Decoder value -> String -> Body -> Task Error value
  , delete  : Decoder value -> String -> Task Error value
  }


type alias MongoDB =
  { create : String -> String
  , find : String -> String
  , get : String -> String
  , getById : String -> String
  }


database : RestBackend -> String -> MongoDB
database rest url =
  { get = (\s -> concat [ url, s ])
  , getById = (\s -> concat [ url, s ])
  , find = (\s -> concat [ url, s ])
  , create = (\s -> concat [ url, s ])
  }
