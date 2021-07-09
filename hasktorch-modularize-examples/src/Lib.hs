{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import qualified Control.Foldl as L
import qualified Data.Text as T
import Debug.Trace
import Text.RawString.QQ
import Turtle
import Turtle.Shell as TS
import Prelude hiding (FilePath)

run :: FilePath -> IO ()
run rootPath = do
  let examplesPath = rootPath </> "examples"
      examplesConfigPath = examplesPath </> "examples" <.> "cabal"
  examplesConfig <- readTextFile examplesConfigPath
  examplePaths <- TS.fold (examples examplesPath) L.list
  sh $ select examplePaths >>= writeExampleCabal examplesConfig
  addPackagesToCabalProject rootPath examplePaths

writeExampleCabal config path = do
  let name = format fp $ basename path
      project = projectConfig name

  case match (has $ project "executable") config of
    [] -> do
      printf ("Couldn't parse config for " % s % "\n") name
      exit $ ExitFailure 1
    exeCfg : _ -> do
      let libCfg = case match (has $ project "library") config of
            [] -> mempty
            libCfg : _ -> libCfg
          cabalFile = path </> fromText name <.> "cabal"
          exampleConfig = commonConfig name <> exeCfg <> libCfg
      printf ("Writing " % s % "\n") name
      liftIO $ writeTextFile cabalFile exampleConfig

addPackagesToCabalProject rootPath packagePaths = do
  let cabalProjectPath = rootPath </> "cabal.project"
  cabalProject <- readTextFile cabalProjectPath
  case match modifyPackagesFunction cabalProject of
    [] -> do
      printf ("Couldn't parse packages section in " % s % "\n") $ format fp cabalProjectPath
      exit $ ExitFailure 1
    modify : _ -> do
      let rootDir = rootPath </> ""
      case traverse (stripPrefix rootDir) packagePaths of
        Just relPaths -> do
          let mkPkg path = format fp (path </> "*" <.> "cabal")
              newPkgs = fmap mkPkg relPaths
          writeTextFile cabalProjectPath $ modify (<> newPkgs)
        Nothing -> fail "impossible case"

modifyPackagesFunction :: Pattern (([Text] -> [Text]) -> Text)
modifyPackagesFunction = do
  before <- selfless chars
  header <- "packages:"
  sep <- "\n" <> spaces
  packages <- selfless chars `sepBy` text sep
  after <- "\n\n" <> chars
  pure
    ( \modify ->
        let newPackages = modify packages
         in mconcat [before, T.intercalate sep (header : newPackages), after]
    )

examples :: FilePath -> Shell FilePath
examples examplesPath = do
  path <- ls examplesPath
  status <- stat path
  guard $ isDirectory status
  hasSrc <- fold (ls path) $ L.any (\p -> extension p == Just "hs")
  guard hasSrc
  pure path

projectConfig :: Text -> Text -> Pattern Text
projectConfig name component =
  "\n"
    <> ( text component
           <> spaces
           <> text name
       )
    <> selfless chars1
    <> "\n" <* (void newline <|> eof)

commonConfig :: Text -> Text
commonConfig name =
  [r|cabal-version:       2.2
name:                |]
    <> name
    <> [r|
version:             0.2.0.0
synopsis:            examples for the new version of hasktorch
-- description:
homepage:            https://github.com/hasktorch/hasktorch#readme
license:             BSD-3-Clause
author:              Hasktorch Contributor Team
maintainer:          hasktorch@gmail.com
copyright:           2019 Austin Huang
category:            Machine Learning
build-type:          Simple

common config
  default-language:    Haskell2010
  ghc-options:         -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver -fplugin GHC.TypeLits.Extra.Solver -fconstraint-solver-iterations=0 -rtsopts
  build-depends:       base >= 4.7 && < 5
                     , hasktorch
                     , ghc-typelits-extra
                     , ghc-typelits-knownnat
                     , ghc-typelits-natnormalise
                     , mtl
|]
