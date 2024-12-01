module HeaderSchema(HeaderSchema) where 

import Data.Aeson (Value, eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Schema
import Data.Maybe (fromJust)

type HeaderSchema = [schema| 
    {
        encoding: Text,
        version: Text
    }
|]
