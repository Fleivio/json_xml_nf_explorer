module SchemaTest(main') where 

import Data.Aeson (Value, eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Schema
import Data.Maybe (fromJust)

main' :: IO ()
main' = do
    str <- B.readFile "files/test.json"
    let d = eitherDecode str :: (Either String Value)
    case d of
        Left err -> putStrLn err
        Right ps -> print $ mkTest ps

mkTest :: Value -> Object SchemaTest
mkTest = fromJust . decode . encode

type SchemaTest = [schema| 
    {
        foo: Text,
        int: Int
    }
|]

