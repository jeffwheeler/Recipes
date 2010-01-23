module Main where

import Data.Recipe.Parser
import Data.Recipe.Structure

main = do
  p <- parseFile "tests/alfredo.recipe"
  case p of
    Left err -> putStrLn err
    Right r  -> writeFile "/home/jeff/Desktop/recipe.html" (recipeGraph r)

