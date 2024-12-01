module XmlToJson(parseXML, transformFile) where

import Text.Parsec.String
import Text.Parsec
import GHC.Generics
import Data.Aeson
import Data.Aeson.Key
import qualified Data.ByteString.Lazy as B

data Tag = Tag String [(String, String)] (Either [Tag] String) deriving(Show, Eq, Generic)

instance ToJSON Tag where
  toJSON (Tag name attrs inner) = object [fromString name .= object ["_attrs" .= attrs, "_content" .= content]]
    where content = case inner of
                      Left tags -> toJSON tags
                      Right str -> toJSON str

transformFile :: FilePath -> FilePath -> IO ()
transformFile input output = do
  inputStr <- readFile input
  let qTag = parseXML inputStr
  
  case qTag of 
    Left err -> print err
    Right tag -> let jsonBs = encode tag
                 in B.writeFile output jsonBs

parseXML :: String -> Either ParseError Tag
parseXML = parse tagParser ""

ignores :: Parser String
ignores = many $ oneOf " \n\t"

tagParser :: Parser Tag
tagParser = do
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
  attrs <- many $ do
    ignores
    attrName <- many1 $ noneOf " \n\t=>"
    ignores
    char '='
    ignores
    char '"'
    attrValue <- many1 $ noneOf "\""
    char '"'
    ignores
    return (attrName, attrValue)
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

innerParser :: Parser (Either [Tag] String)
innerParser = (Left <$> bodyParser) <|> (Right <$> pcdataParser)

bodyParser :: Parser [Tag]
bodyParser = many1 (try tagParser)

pcdataParser :: Parser String
pcdataParser = many (noneOf "<")