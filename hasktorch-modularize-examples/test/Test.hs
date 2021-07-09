{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.Text as T
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ
import Turtle

main = defaultMain tests

tests =
  testGroup
    "Unit tests"
    [ testCase
        "Parse cabal.project"
        let parsed = match appendPackageFunction exampleCabalProject
         in assertBool "parses successfully" $ not (null parsed)
    ]

exampleCabalProject :: Text
exampleCabalProject =
  [r|packages:
    codegen/*.cabal
    libtorch-ffi/*.cabal
    libtorch-ffi-helper/*.cabal
    hasktorch/*.cabal
    examples/*.cabal
    experimental/*.cabal

write-ghc-environment-files: always
tests: true
documentation: false

allow-newer: doctest-0.17:ghc, pipes-text-0.0.2.5:pipes-safe, pipes-text-0.0.2.5:streaming-commons

|]
