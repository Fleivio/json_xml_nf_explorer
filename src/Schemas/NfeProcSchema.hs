module Schemas.NfeProcSchema(NfeProcSchema) where 

import Data.Aeson.Schema

type NfeProcSchema = [schema| 
  {
    nfeProc: {
    _attrs: {
      versao: Text,
      xmlns: Text
    },
    _content: {
      _a: Text
    }
  }
  }
|]
