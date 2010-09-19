module Data.Recipe.Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

import Data.Recipe.Structure

parseFile filename = do
  p <- parseFromFile recipeP filename
  return $ case p of
    Left err -> Left  $ show err
    Right s  -> Right $ s

recipeP = do optional $ newlines
             r <- indentedRecipeP ""
             eof
             return r

indentedRecipeP indent =
  try (ingredientP indent) <|> try (oneLineResultP indent) <|> resultP indent

resultP indent = do
  -- Match item title
  n <- afterIndent indent $ do
         n <- nameP
         char ':'
         return n

  -- Find new indentation level
  indent' <- lookAhead $ do
    a <- string indent
    b <- many1 $ char ' '
    return $ a ++ b

  -- Match ingredients/sub-results
  ps <- many1 $ (try (indentedRecipeP indent') <?> "recipe")

  -- Match actions
  (a:as) <- many1 $ (try (afterIndent indent' stringP) <?> "action")

  let base = Result {name = Nothing, action = a, pieces = ps, yield = 2} in
    return $ (foldl startingWith base as) { name = Just n }

oneLineResultP indent =
  afterIndent indent $ do
    n <- nameP
    char ':'
    as <- many1 $ do many1 $ char ' '
                     stringP

    let base = Ingredient 1 n
    return $ foldl startingWith base as

ingredientP indent = do
  n <- afterIndent indent nameP
  return $ Ingredient 1 n

afterIndent indent p = between (string indent) (newlines) p

newlines = many1 $ try $ do
  many $ char ' '
  optional $ do
    string "--"
    many $ noneOf "\n" 
  newline

nameP = p <?> "name"
  where p = do h <- alphaNum <|> char '~' -- Let names start with ~
               t <- many $ alphaNum <|> oneOf [' ', '_', '/', ',', '(', ')', '~']
               return $ h : t

-- This is complicated to let ' or " be used
stringP = p <?> "string"
    where p = do q <- oneOf "'\""
                 s <- many $ noneOf $ q:"\n\t"
                 char q
                 return s

