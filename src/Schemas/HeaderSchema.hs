module Schemas.HeaderSchema(HeaderSchema) where 

import Data.Aeson.Schema

type HeaderSchema = [schema| 
    {
        encoding: Text,
        version: Text
    }
|]
