module XmlToJson(fileToJSON) where

import Text.Parsec.String
import Text.Parsec
import GHC.Generics
import Data.Aeson
import Data.Aeson.Key
import qualified Data.ByteString.Lazy as B

fileToJSON :: FilePath -> FilePath -> IO ()
fileToJSON input output = do
  inputStr <- readFile input
  let qTag = parseXML inputStr
  
  case qTag of 
    Left err -> print err
    Right tag -> let jsonBs = encode tag
                 in B.writeFile output jsonBs

-----AESON------------------------------------------------------------

data Tag = Tag String [(String, String)] (Either [Tag] String) deriving(Show, Eq, Generic)
data Xml = Xml [(String, String)] Tag deriving(Show, Eq, Generic)

instance ToJSON Tag where
  toJSON (Tag name attrs inner) =
      object [fromString name .= body]
    where body = object $ atrs ++ innerToVals inner
          atrs = if null attrs then [] else ["_attrs" .= object (attrsToVals attrs)]

instance ToJSON Xml where
  toJSON (Xml header tag) = object ["_header" .= headerObj, "_root" .= tagObj]
    where headerObj = object $ map (\(k,v) -> fromString k .= v) header
          tagObj = toJSON tag

innerToVals :: Either [Tag] String -> [(Key, Value)]
innerToVals (Left tags) = tagsToVals tags
innerToVals (Right str) = ["pcdata" .= str]

tagsToVals :: [Tag] -> [(Key, Value)]
tagsToVals [] = []
tagsToVals (Tag name attrs inner :ts) 
  = (fromString name) .= object (atrs ++ innerToVals inner)
  : tagsToVals ts
  where 
    atrs = if null attrs then [] else ["_attrs" .= object (attrsToVals attrs)]

attrsToVals :: [(String, String)] -> [(Key, Value)]
attrsToVals [] = []
attrsToVals ((k,v):as) = (fromString k .= v) : attrsToVals as

---PARSER-------------------------------------------------------------

parseXML :: String -> Either ParseError Xml
parseXML = parse xmlParser ""

xmlParser :: Parser Xml
xmlParser = do
  ignores
  header <- headerParser
  tag <- tagParser
  eof
  return $ Xml header tag

headerParser :: Parser [(String,String)]
headerParser = do
  string "<?xml"
  attrs <- many attrParser
  string "?>"
  return attrs

ignores :: Parser String
ignores = many $ oneOf " \n\t"

tagParser :: Parser Tag
tagParser = 
  try emptyTag <|> contentTag
  where 
    emptyTag = do
      ignores
      char '<'
      ignores
      name <- many1 $ noneOf " \n\t/>"
      attrs <- many attrParser
      string "/>"
      ignores
      return $ Tag name attrs (Right "")
    contentTag = do 
      ignores
      (name, attrs) <- openTag 
      inner <- innerParser
      closeTag name
      ignores
      return $ Tag name attrs inner
  
openTag :: Parser (String, [(String, String)])
openTag = do
  char '<'
  ignores
  name <- many1 $ noneOf " \n\t/>"
  attrs <- many attrParser
  char '>'
  return (name, attrs)

closeTag :: String -> Parser ()
closeTag name = do
  string "</"
  ignores
  string name
  ignores
  char '>'
  return ()

attrParser :: Parser (String, String)
attrParser = do
  ignores
  name <- many1 $ noneOf " \n\t=?/>"
  ignores
  char '='
  ignores
  char '"'
  value <- many1 $ noneOf "\""
  char '"'
  ignores
  return (name, value)

innerParser :: Parser (Either [Tag] String)
innerParser = (Left <$> bodyParser) <|> (Right <$> pcdataParser)

bodyParser :: Parser [Tag]
bodyParser = many1 (try tagParser) 

pcdataParser :: Parser String
pcdataParser = many (noneOf "<")