import Control.Monad

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser g) = Parser $ \s ->
       fmap (\(a, s') -> (f a, s')) (g s)

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

char :: Char -> Parser ()
char c = Parser $ \s -> case s of
  []    -> Nothing
  c':cs -> if c == c' then Just ((), cs) else Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
  Nothing -> g s
  x       -> x

string :: String -> Parser ()
string = mapM_ char



f :: Int -> Parser ()
f n = replicateM_ n (string "foo" <|> string "bar") >> 
    replicateM_ n (char 'x') >>
    eof

t = [runParser (f 2) "foofooxx" == Just ((),""),
    runParser (f 3) "barfoobarxxx" == Just ((),""),
    runParser (f 0) "foox" == Nothing,
    runParser (f 2) "foobarxxxx" == Nothing]