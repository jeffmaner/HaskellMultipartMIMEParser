module MultipartMIMEParser
    (Content (..),
     Header (..),
     Post (..),
     boundary, -- TODO: Testing only.
     parseContent,
     parseHeader,
     parseHeaders,
     parsePost,
     parsePosts) where

import Control.Applicative ((<$>), (<*>), (<*))
import Data.Char (toLower)
import Text.ParserCombinators.Parsec hiding (Line)

data Header = Header { hName  :: String
                     , hValue :: String
                     , hAddl  :: [(String,String)] } deriving (Eq, Show)

data Content a = Content a | Posts [Post a] deriving (Eq, Show)

data Post a = Post { pHeaders :: [Header]
                   , pContent :: [Content a] } deriving (Eq, Show)

parseHeader :: String -> Header
parseHeader s = case parse header "" s of
                  Left  e -> error $ "Error parsing header.\n" ++ show e
                  Right r -> r

parseHeaders :: String -> [Header]
parseHeaders s = case parse headers "" s of
                   Left  e -> error $ "Error parsing headers.\n" ++ show e
                   Right r -> r

parseContent :: String -> Content String
parseContent s = case parse content "" s of
                   Left  e -> error $ "Error parsing content.\n" ++ show e
                   Right r -> r

parsePost :: String -> Post String
parsePost s = case parse post "" s of
                Left  e -> error $ "Error parsing post.\n" ++ show e
                Right r -> r

parsePosts :: String -> Content String
parsePosts s = case parse posts "" s of
                Left  e -> error $ "Error parsing post.\n" ++ show e
                Right r -> r

posts :: Parser (Content String)
posts = Posts <$> many post

post :: Parser (Post String)
post = do
  hs <- headers
  c  <- case boundary hs of
         "" -> content >>= \s->return [s]
         b  -> newline >> (string b) >> newline >>
              manyTill content (string b) -- >>= \cs->return $ liftM post cs
  return $ Post { pHeaders=hs, pContent=c }
  -- where
boundary hs = case lookup "boundary" $ concatMap (namesToLower . hAddl) hs of
                Just b  -> "--" ++ b
                Nothing -> ""
  where nameToLower (n,v) = (map toLower n, v)
        namesToLower = map nameToLower

-- Debugging:
everything :: Parser String
everything = manyTill anyChar eof

content :: Parser (Content String)
content = do
  xs <- manyTill line blankField
  return $ Content $ unlines xs
  where line = manyTill anyChar newline

headers :: Parser [Header]
headers = manyTill header blankField

blankField = newline

header :: Parser Header
header =
    Header <$> fieldName  <* string ":"
           <*> fieldValue <* optional (try newline)
           <*> nameValuePairs
  where fieldName = many $ noneOf ":"
        fieldValue = spaces >> many (noneOf "\r\n;")
        nameValuePairs = option [] $ many nameValuePair

nameValuePair :: Parser (String,String)
nameValuePair = do
  try $ do n <- name
           v <- value
           return $ (n,v)

name :: Parser String
name = string ";" >> spaces >> many (noneOf "=")

value :: Parser String
value = string "=" >> between quote quote (many (noneOf "\r\n;\""))
  where quote = string "\""
