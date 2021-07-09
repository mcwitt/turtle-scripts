{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib (run)
import Turtle

main = do
  rootPath <- options "Modularize examples" $ argPath "path" "Hasktorch path"
  run rootPath
