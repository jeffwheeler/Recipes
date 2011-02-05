{-# LANGUAGE TemplateHaskell, TypeFamilies, QuasiQuotes #-}
module Main where

import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld where
  approot _ = ""

getHomeR  = defaultLayout [hamlet|Hello, world!|]

main = basicHandler 3000 HelloWorld
