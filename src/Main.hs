module Main where

import Text.Parsec
import Text.Parsec.String

data JSONValue = 
    B Bool
    | S String
    | A [JSONValue]
    | O [(String, JSONValue)]
    | N Integer
    deriving Show

jsonValue :: Parser JSONValue
jsonValue =
    jsonBool 
    <|> jsonString 
    <|> jsonArray 
    <|> jsonObject 
    <|> jsonInt <* spaces

boolTrue :: Parser Bool
boolTrue = (string "true") *> pure True

boolFalse :: Parser Bool
boolFalse = (string "false") *> pure False

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser JSONValue
jsonBool = lexeme (B <$> bool)

stringLiteral :: Parser String
stringLiteral = 
    char '"' *> (many (noneOf ['"'])) <* char '"' 

jsonString :: Parser JSONValue
jsonString = lexeme (S <$> stringLiteral)

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit

jsonInt :: Parser JSONValue
jsonInt = lexeme (N <$> int)

array :: Parser [JSONValue]
array = 
    lexeme (char '[')
    *>
    (jsonValue `sepBy` (lexeme comma))
    <*
    (char ']')

jsonArray :: Parser JSONValue
jsonArray = lexeme (A <$> array)

objectEntry :: Parser (String, JSONValue)
objectEntry = do
    key <- stringLiteral
    spaces
    char ':'
    spaces
    value <- jsonValue
    return (key, value)

object :: Parser [(String, JSONValue)]
object =
    lexeme (char '{')
    *> 
    (objectEntry `sepBy` (lexeme comma))
    <* 
    char '}'

jsonObject :: Parser JSONValue
jsonObject =
    lexeme (O <$> object)



ws :: Parser String
ws = many (oneOf " \t\n")

comma :: Parser Char
comma = (char ',')

-- A parser combinator which skips whitespaces from both sides
lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

main :: IO ()
main = do
    result <- parseFromFile jsonValue "test.json"
    case result of
        Left err  -> print err
        Right xs  -> print xs