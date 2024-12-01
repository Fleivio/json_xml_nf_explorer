module MainSchema(main') where 

import Data.Aeson (Value, eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Schema
import Data.Maybe (fromJust)
import Schemas.RootSchema
import Schemas.HeaderSchema

main' :: IO ()
main' = do
    str <- B.readFile "files/test.json"
    let d = eitherDecode str :: (Either String Value)
    case d of
        Left err -> putStrLn err
        Right ps -> print $ mkTest ps

mkTest :: Value -> Object MySchema
mkTest = fromJust . decode . encode

type MySchema = [schema| 
    {
        _header: #HeaderSchema,
        _root: #RootSchema
    }
|]