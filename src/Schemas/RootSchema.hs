module Schemas.RootSchema(RootSchema) where

import Data.Aeson.Schema
import Schemas.NfeProcSchema

type RootSchema = [schema|
{
    nfeProc: #NfeProcSchema
}
|]