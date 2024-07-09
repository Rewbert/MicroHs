module Text.SerokellParser (
      Error(..)
    , Parser(..)
    , runParser
    , satisfy
    , always
    , optional
    , alt
    , remainder
    , many
    , some
    , sepBy
    , sepBy1
    , count
    , manyTill
    , manyTill_
    , someTill
    , someTill_
    , char
    , string
    , line) where

-- import Control.Applicative (Alternative(..))
import Data.List (nub)

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i [i] -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)

{-| i: input stream
    e: type of custom error messages
    a: result of the parsing function -}
data Parser i e a = Parser ([i] -> Either [Error i e] (a, [i]))

runParser :: Parser i e a -> [i] -> Either [Error i e] (a, [i])
runParser (Parser i) is = i is

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input ->
    case f input of
      Left err -> Left err
      Right (f', rest) ->
        case p rest of
          Left err -> Left err
          Right (output, rest') -> Right (f' output, rest')

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) ->
        let
          Parser p' = k output
        in
        p' rest

-- * Primitives

-- | Test the current token with a predicate, and return the consumed character
satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [Unexpected hd rest]

-- | Always parse a single token
always :: Parser i e i
always = Parser $ \input ->
    case input of
        [] -> Left [EndOfInput]
        hd : rest -> Right (hd, rest)

-- | Run a parser that might fail, but without throwing an error
optional :: Parser i e a -> Parser i e (Maybe a)
optional (Parser p) = Parser $ \input ->
    case p input of
        Right (output, rest) -> Right (Just output, rest)
        Left _ -> Right (Nothing, input)

-- | Alternative (if I import Control.Applicative and give the instance, I get some arcane error from MHS)
alt :: (Eq i, Eq e) => Parser i e a -> Parser i e a -> Parser i e a
alt (Parser l) (Parser r) = Parser $ \input ->
    case l input of
        Right res -> Right res
        Left e -> case r input of
            Right res -> Right res
            Left e1 -> Left $ nub $ e <> e1

remainder :: Parser i e [i]
remainder = Parser $ \input -> Right (input, input)

-- * Derives combinators

-- | Run the given parser zero or more times
many :: Parser i e a -> Parser i e [a]
many p = go id
  where
    go f = do
      r <- optional p
      case r of
        Nothing -> return (f [])
        Just x -> go (f . (x :))

-- | Run the given parser one or more times
some :: Parser i e a -> Parser i e [a]
some p = do
    r <- p
    rs <- Text.SerokellParser.many p
    return $ r : rs

-- | Run the given parser, separated by sep, zero or more times
sepBy :: Parser i e a -> Parser i e b -> Parser i e [a]
sepBy p sep = do
    r <- optional p
    case r of
        Nothing -> return []
        Just x -> (x :) <$> Text.SerokellParser.many (sep >> p)

-- | RUn the given parser, separated by sep, one or more times
sepBy1 :: Parser i e a -> Parser i e b -> Parser i e [a]
sepBy1 p sep = do
    r <- p
    _ <- sep
    rs <- sepBy p sep
    return $ r : rs

-- | Run a parser n amount of times
count :: Int -> Parser i e a -> Parser i e [a]
count 0 _ = return []
count n p = do
    r <- p
    rs <- count (n-1) p
    return $ r : rs

-- | Run a parser zero or more times until end parses. Discards the end.
manyTill :: Parser i e a -> Parser i e b -> Parser i e [a]
manyTill p end = fst <$> manyTill_ p end

-- | Run a parser zero or more times until end parses. Returns the end.
manyTill_ :: Parser i e a -> Parser i e b -> Parser i e ([a], b)
manyTill_ p end = go id
  where
    go f = do
        done <- optional end
        case done of
            Just done' -> return (f [], done')
            Nothing -> do
                x <- p
                go (f . (x :))

-- | Run a parser one or more times until end parses. Discards the end.
someTill :: Parser i e a -> Parser i e b -> Parser i e [a]
someTill p end = do
    r <- p
    rs <- manyTill p end
    return $ r : rs

-- | Run a parser one or more times until end parses. Returns the end.
someTill_ :: Parser i e a -> Parser i e b -> Parser i e ([a], b)
someTill_ p end = do
    r <- p
    (rs, e) <- manyTill_ p end
    return $ (r : rs, e)

-- | Parse a specific character
char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

-- | Parse a specific string
string :: Eq i => [i] -> Parser i e [i]
string [] = pure []
string (x : xs) = (:) <$> char x <*> string xs

line :: Parser Char e [Char]
line = fst <$> manyTill_ always (char '\n')

-- numbers :: Parser Char e [Char]
-- numbers 