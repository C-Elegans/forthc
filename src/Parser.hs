module Parser where
import qualified Data.Text as T
import Data.Char
import Control.Applicative hiding (many)

type Error = String
newtype Parser a = P { runParser :: T.Text -> (T.Text, Either Error a)}
instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a) -> (res, Right (f a))

instance Applicative Parser where
  pure a = P (\stream -> (stream, Right a))
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)
instance Monad Parser where
  return = pure
  fail errmsg = P $ \txt -> (txt, Left errmsg)

  (P parse) >>= next =
    P $ \text ->
    let (left, res) = parse text
    in case res of
      Left err -> (left, Left err)
      Right val -> runParser (next val) left
instance Alternative Parser where
    empty = P $ \txt -> (txt, Left "Parsing failed!")
    (P pa) <|> otherParser =
        P $ \txt ->
        case pa txt of
          full@(_, Right _) -> full
          _ -> runParser otherParser txt

int :: Parser Int
int = fmap (read . T.unpack) numStarter
  where numStarter = do
          optneg <- optional (char '-')
          rest <- satisfy1 isDigit
          pure $ maybe rest (`T.cons` rest) optneg
  
  

char :: Char -> Parser Char
char c =
  P $ \txt ->
  case T.uncons txt of
    Just (firstC, rest) | firstC == c -> (rest, Right c)
    _ -> (txt, Left $ "Expected a " ++ (show c))
  
satisfy f =
  P $ \txt ->
  let (matches, rest) = T.span f txt
  in (rest, Right matches)

satisfy1 f =
  satisfy f >>= \res ->
                  if (T.null res) then
                    fail "Expecting some data"
                  else
                    pure res
spaces = satisfy isSpace >> return ()


isToken ';' = False
isToken ':' = False
isToken c | isSpace c = False
isToken _ = True

string :: Parser T.Text
string = spaces >>= \_ ->  
  P $ \txt ->
  case T.uncons txt of
    Just (firstC, rest) | isToken firstC ->
                            let (str, rest') = T.span isToken rest
                            in (rest', Right $ firstC `T.cons` str)
    _ -> (txt, Left $ "Expecting a string")
                          

many :: Parser a -> Parser [a]
many p =
  P $ \txt ->
  let (rest, res) = runParser p txt
  in case res of
    Right a -> case runParser (many1 p) rest of
      (rest', Right as) -> (rest', Right (a:as))
      _ -> (rest, Right (a:[]))
    Left err ->
      (txt, Right [])

many1 p = do
  a <- p
  as <- many p
  return (a:as)

identifier :: T.Text -> Parser T.Text
identifier t =
    P $ \txt ->
    let tlen = T.length t
    in if T.take tlen txt == t
       then (T.drop tlen txt, Right t)
       else (txt, Left $ "Expected " ++ show t)
