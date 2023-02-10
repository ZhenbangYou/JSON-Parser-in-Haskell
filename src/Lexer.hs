module Lexer (Punctuation (..), Value, Token (..), lexer) where

import Data.Char (isDigit)

data Space = Space
  deriving (Show)

data Punctuation = Comma | Colon | BraceStart | BraceEnd | BracketStart | BracketEnd
  deriving (Show)

data Value = JSString String | JSNumber Double | JSBoolean Bool | JSNull

data Token = Punc Punctuation | Val Value
  deriving (Show)

instance Show Value where
  show (JSString s) = s
  show (JSNumber d) = show d
  show (JSBoolean b) = show b
  show JSNull = "null"

parseSpace :: String -> Maybe (Space, String)
parseSpace (' ' : xs) = Just (Space, xs)
parseSpace ('\n' : xs) = Just (Space, xs)
parseSpace ('\r' : xs) = Just (Space, xs)
parseSpace ('\t' : xs) = Just (Space, xs)
parseSpace _ = Nothing

parseComma :: String -> Maybe (Punctuation, String)
parseComma (',' : xs) = Just (Comma, xs)
parseComma _ = Nothing

parseColon :: String -> Maybe (Punctuation, String)
parseColon (':' : xs) = Just (Colon, xs)
parseColon _ = Nothing

parseBraceStart :: String -> Maybe (Punctuation, String)
parseBraceStart ('{' : xs) = Just (BraceStart, xs)
parseBraceStart _ = Nothing

parseBraceEnd :: String -> Maybe (Punctuation, String)
parseBraceEnd ('}' : xs) = Just (BraceEnd, xs)
parseBraceEnd _ = Nothing

parseBrackectStart :: String -> Maybe (Punctuation, String)
parseBrackectStart ('[' : xs) = Just (BracketStart, xs)
parseBrackectStart _ = Nothing

parseBrackectEnd :: String -> Maybe (Punctuation, String)
parseBrackectEnd (']' : xs) = Just (BracketEnd, xs)
parseBrackectEnd _ = Nothing

-- consume both quote
parseString :: String -> Maybe (Value, String)
parseString ('"' : s) = case parseString' s of
  (t, '"' : xs) -> Just (JSString t, xs)
  _ -> Nothing
  where
    -- don't consume right quote
    parseString' :: String -> (String, String)
    parseString' ('\\' : x : xs) = case parseString' xs of
      (ss, xs2) -> ('\\' : x : ss, xs2)
    parseString' xs@('"' : _) = ("", xs)
    parseString' (x : xs) = case parseString' xs of
      (ss, xs2) -> (x : ss, xs2)
    parseString' xs = ("", xs)
parseString _ = Nothing

parseUnsignedInteger :: String -> Maybe (Integer, String)
parseUnsignedInteger s =
  let number = takeWhile isDigit s
      xs = dropWhile isDigit s
   in case number of
        "" -> Nothing
        n -> Just (read n :: Integer, xs)

parseInteger :: String -> Maybe (Integer, String)
parseInteger ('+' : s) = parseUnsignedInteger s
parseInteger ('-' : s) = case parseUnsignedInteger s of
  Just (i, xs) -> Just (-i, xs)
  Nothing -> Nothing
parseInteger s = parseUnsignedInteger s

parseDoubleWithoutExp :: String -> Maybe (Double, String)
parseDoubleWithoutExp ('.' : xs) = case parseUnsignedInteger xs of
  Just (i2, xxs) -> Just (read ("0." ++ show i2) :: Double, xxs)
  _ -> Nothing
parseDoubleWithoutExp ('+' : '.' : xs) = case parseUnsignedInteger xs of
  Just (i2, xxs) -> Just (read ("0." ++ show i2) :: Double, xxs)
  _ -> Nothing
parseDoubleWithoutExp ('-' : '.' : xs) = case parseUnsignedInteger xs of
  Just (i2, xxs) -> Just (read ("-0." ++ show i2) :: Double, xxs)
  _ -> Nothing
parseDoubleWithoutExp s = case parseInteger s of
  Nothing -> Nothing
  Just (i, '.' : xs) -> case parseUnsignedInteger xs of
    Just (i2, xxs) -> Just (read (show i ++ "." ++ show i2) :: Double, xxs)
    Nothing -> Just (fromInteger i, xs)
  Just (i, xs) -> Just (fromInteger i, xs)

parseDouble' :: String -> Maybe (Double, String)
parseDouble' s = case parseDoubleWithoutExp s of
  Nothing -> Nothing
  Just (d, 'e' : xs) -> case parseInteger xs of
    Just (i, xxs) -> Just (read (show d ++ "e" ++ show i), xxs)
    Nothing -> Nothing
  Just (d, 'E' : xs) -> case parseInteger xs of
    Just (i, xxs) -> Just (read (show d ++ "e" ++ show i), xxs)
    Nothing -> Nothing
  res -> res

parseDouble :: String -> Maybe (Value, String)
parseDouble s = case parseDouble' s of
  Just (d, xs) -> Just (JSNumber d, xs)
  Nothing -> Nothing

parseBool :: String -> Maybe (Value, String)
parseBool ('t' : 'r' : 'u' : 'e' : xs) = Just (JSBoolean True, xs)
parseBool ('f' : 'a' : 'l' : 's' : 'e' : xs) = Just (JSBoolean False, xs)
parseBool _ = Nothing

parseNull :: String -> Maybe (Value, String)
parseNull ('n' : 'u' : 'l' : 'l' : xs) = Just (JSNull, xs)
parseNull _ = Nothing

lexerPunctuation :: String -> Maybe (Punctuation, String)
lexerPunctuation "" = Nothing
lexerPunctuation s =
  case parseSpace s of
    Just (_, xs) -> lexerPunctuation xs
    Nothing ->
      let lexers = [parseComma, parseColon, parseBraceStart, parseBraceEnd, parseBrackectStart, parseBrackectEnd]
       in foldl
            ( \b a -> case b of
                Just _ -> b
                Nothing -> a s
            )
            Nothing
            lexers

lexerValue :: String -> Maybe (Value, String)
lexerValue "" = Nothing
lexerValue s =
  case parseSpace s of
    Just (_, xs) -> lexerValue xs
    Nothing ->
      let lexers = [parseString, parseDouble, parseBool, parseNull]
       in foldl
            ( \b a -> case b of
                Just _ -> b
                Nothing -> a s
            )
            Nothing
            lexers

lexOne :: String -> Maybe (Token, String)
lexOne "" = Nothing
lexOne s =
  case lexerPunctuation s of
    Just (p, xs) -> Just (Punc p, xs)
    Nothing -> case lexerValue s of
      Just (t, xs) -> Just (Val t, xs)
      Nothing -> Nothing

lexAll :: String -> ([Token], String)
lexAll "" = ([], "")
lexAll s = case lexOne s of
  Just (t, xs) ->
    let (ts, xxs) = lexAll xs
     in (t : ts, xxs)
  Nothing -> ([], s)

lexer :: String -> [Token]
lexer s = case lexAll s of
  (ts, "") -> ts
  _ -> error "Some token can't be lexed!"