{-# LANGUAGE OverloadedStrings #-}
module Parse
  ( Token(..)
  , WordDef(..)
  , parse)where
import Prelude hiding (words)
import Parser
import qualified Data.Text as T
import Data.Char
import Control.Applicative hiding (many)

data WordDef = WordDef { name :: T.Text
                       , words :: [Token]
                       }
             deriving(Show)

data Token = Number Int
           | Word T.Text
           | Emit T.Text
           | EmitLow T.Text
           deriving(Show)


parsestr :: String -> Either Error [WordDef]
parsestr = parse . T.pack
parse :: T.Text -> Either Error [WordDef]
parse t =
  let (_, res) = runParser (many parsedef >>= (return . concat)) t
  in res
  
parsedef :: Parser [WordDef]
parsedef = do
  spaces
  char ':'
  spaces
  name <- string
  spaces
  toks <- many token
  let w = WordDef{name=name, words=toks}
  spaces
  char ';'
  return [w]
  <|> do
    identifier "include" 
    spaces
    file <- string
    includefile file
    return []

includefile :: T.Text -> Parser ()
includefile t = return ()
    

token :: Parser Token
token = spaces >> (do
  n <- int
  return $ Number n)
  <|> do
    char '{'
    parseEmit
  <|> do
    str <- string
    return $ Word str


parseEmit :: Parser Token
parseEmit = (do
      char 'l'
      spaces
      txt <- satisfy (\c -> c /= '}')
      char '}'
      return $ EmitLow txt)
            <|> (do
      spaces
      txt <- satisfy (\c -> c /= '}')
      char '}'
      return $ Emit txt)
      
  
