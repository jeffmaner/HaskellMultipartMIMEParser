module MultipartMIMEParser
    (Header (..),
     Post (..),
     boundary, -- TODO: Testing only.
     parseContent,
     parseHeader,
     parseHeaders,
     parsePost,
     parsePosts) where

import Control.Applicative ((<$>), (<*>), (<*))
import Text.ParserCombinators.Parsec hiding (Line)

data Header = Header { hName  :: String
                     , hValue :: String
                     , hAddl  :: [(String,String)] } deriving (Eq, Show)

{-data Content = Content String
 -             | Post { pHeaders :: [Header]
 -                    , pContent :: Content } deriving (Eq, Show)
 -}
data Post = Post { pHeaders :: [Header]
                 , pContent :: [String] } deriving (Eq, Show)

parseHeader :: String -> Header
parseHeader s = case parse header "" s of
                  Left  e -> error $ "Error parsing header.\n" ++ show e
                  Right r -> r

parseHeaders :: String -> [Header]
parseHeaders s = case parse headers "" s of
                   Left  e -> error $ "Error parsing headers.\n" ++ show e
                   Right r -> r

parseContent :: String -> String
parseContent s = case parse content "" s of
                   Left  e -> error $ "Error parsing content.\n" ++ show e
                   Right r -> r

parsePost :: String -> Post
parsePost s = case parse post "" s of
                Left  e -> error $ "Error parsing post.\n" ++ show e
                Right r -> r

parsePosts :: String -> [Post]
parsePosts s = case parse posts "" s of
                Left  e -> error $ "Error parsing post.\n" ++ show e
                Right r -> r

posts :: Parser [Post]
posts = many post

post :: Parser Post
post = do -- Post <$> headers <*> many content
  hs <- headers
  c  <- case boundary hs of
         "" -> content >>= \s->return [s]
         b  -> newline >> (string b) >> newline >>
              manyTill content (string b) >>= \xs->return [unlines xs]
         -- b  -> newline >> (string b) >> newline >> everything `endBy` (string b)
  return $ Post { pHeaders=hs, pContent=c }
  -- where
boundary hs = case lookup "boundary" $ concatMap hAddl hs of
                Just b  -> "--" ++ b
                Nothing -> ""
        -- TODO: lookup "boundary" needs to be case-insensitive.

-- Debugging:
everything :: Parser String
everything = manyTill anyChar eof

content :: Parser String
content = do
  xs <- manyTill line blankField
  return $ unlines xs
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
