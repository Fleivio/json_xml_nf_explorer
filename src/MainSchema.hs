module SchemaTest(main') where 

import Data.Aeson (Value, eitherDecode, encode, decode)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Schema
import Data.Maybe (fromJust)
import HeaderSchema

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
        _root: {
            _nfeProc: {
                _attrs: {
                    versao: Text
                },
                _content: {
                    nFe: Text
                }
            }
        }
    }
|]