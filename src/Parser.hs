module Parser (parser) where

import Lexer

data Record = Record Value AnyType

data Object = EmptyObject | ObjectNode Record Object

data Array = EmptyArray | ArrayNode AnyType Array

data AnyType = ValueType Value | ObjectType Object | ArrayType Array

instance Show Record where
  show (Record v a) = "(" ++ show v ++ ", " ++ show a ++ ")"

instance Show Object where
  show o = "{" ++ objToStr o ++ "}"
    where
      objToStr EmptyObject = ""
      objToStr (ObjectNode r o2) = show r ++ ", " ++ objToStr o2

instance Show Array where
  show a = "[" ++ arrToStr a ++ "]"
    where
      arrToStr EmptyArray = ""
      arrToStr (ArrayNode x xs) = show x ++ ", " ++ arrToStr xs

instance Show AnyType where
  show (ValueType v) = show v
  show (ObjectType o) = show o
  show (ArrayType a) = show a

parseRecord :: [Token] -> Maybe (Record, [Token])
parseRecord (Val key : Punc Colon : xs) =
  case parseAnyType xs of
    Just (a, xxs) -> Just (Record key a, xxs)
    Nothing -> Nothing
parseRecord _ = Nothing

-- don't consume '}'
parseObjectNode :: [Token] -> Maybe (Object, [Token])
parseObjectNode [] = Nothing
parseObjectNode xs@(Punc BraceEnd : _) = Just (EmptyObject, xs)
parseObjectNode xs = case parseRecord xs of
  Just (r, xxs@(Punc BraceEnd : _)) -> Just (ObjectNode r EmptyObject, xxs)
  Just (r, Punc Comma : xxs) -> case parseObjectNode xxs of
    Just (obj, xxxs) -> Just (ObjectNode r obj, xxxs)
    Nothing -> Nothing
  _ -> Nothing

parseObject :: [Token] -> Maybe (Object, [Token])
parseObject (Punc BraceStart : Punc BraceEnd : xs) = Just (EmptyObject, xs)
parseObject (Punc BraceStart : xs) =
  case parseObjectNode xs of
    Just (obj, Punc BraceEnd : xxs) -> Just (obj, xxs)
    _ -> Nothing
parseObject _ = Nothing

-- don't consume '['
parseArrayNode :: [Token] -> Maybe (Array, [Token])
parseArrayNode [] = Nothing
parseArrayNode xs@(Punc BracketEnd : _) = Just (EmptyArray, xs)
parseArrayNode xs = case parseAnyType xs of
  Just (a, xxs@(Punc BracketEnd : _)) -> Just (ArrayNode a EmptyArray, xxs)
  Just (a, Punc Comma : xxs) -> case parseArrayNode xxs of
    Just (arr, xxxs) -> Just (ArrayNode a arr, xxxs)
    Nothing -> Nothing
  _ -> Nothing

parseArray :: [Token] -> Maybe (Array, [Token])
parseArray (Punc BracketStart : Punc BracketEnd : xs) = Just (EmptyArray, xs)
parseArray (Punc BracketStart : xs) =
  case parseArrayNode xs of
    Just (arr, Punc BracketEnd : xxs) -> Just (arr, xxs)
    _ -> Nothing
parseArray _ = Nothing

parseAnyType :: [Token] -> Maybe (AnyType, [Token])
parseAnyType (Val val : xs) = Just (ValueType val, xs)
parseAnyType xs@(Punc BraceStart : _) = case parseObject xs of
  Just (obj, xxs) -> Just (ObjectType obj, xxs)
  Nothing -> Nothing
parseAnyType xs@(Punc BracketStart : _) = case parseArray xs of
  Just (arr, xxs) -> Just (ArrayType arr, xxs)
  Nothing -> Nothing
parseAnyType _ = Nothing

parser :: [Token] -> Object
parser xs = case parseObject xs of
  Just (obj, []) -> obj
  _ -> error "Some token can't be parsed!"