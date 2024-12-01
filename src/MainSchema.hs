module MainSchema(testSchema) where 

import Data.Aeson (Value, eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Schema
import Schemas.RootSchema
import Schemas.HeaderSchema

default(String)

testSchema :: FilePath -> IO ()
testSchema path = do
    str <- B.readFile ("files/" ++ path)
    let d = eitherDecode str :: (Either String Value)
    case d of
        Left err -> putStrLn err
        Right _ -> print "Success"

mkTest :: Value -> Maybe (Object MySchema)
mkTest = decode . encode

type MySchema = [schema| 
    {
        _header: #HeaderSchema,
        _root: #RootSchema
    }
|]

type TestSchema = [schema|
    {
        foo: Text,
        int: Int
    }
|]