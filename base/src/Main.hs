module Main where

import System.Environment
import System.FilePath

import Data.Recipe.Parser
import Data.Recipe.Structure

handleFile :: String -> IO ()
handleFile fn = do
  let targetfn = replaceExtension fn "html"
  p <- parseFile fn
  case p of
    Left err -> putStrLn err
    Right r  -> writeFile targetfn (recipeGraph r)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Error: no filename provided"
    _  -> mapM_ handleFile args
