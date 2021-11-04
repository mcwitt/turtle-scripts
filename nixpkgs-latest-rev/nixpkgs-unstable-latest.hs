#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p curl "haskellPackages.ghcWithPackages (ps: with ps; [ lens-aeson text turtle ])"

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens.Fold as F
import Control.Lens.Prism
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text.IO as TIO
import Turtle

statusUrl = "https://monitoring.nixos.org/prometheus/api/v1/query?query=channel_revision"

main =
  inproc "curl" [statusUrl] mempty
    & single
    <&> lineToText >>= \text ->
      let Just rev =
            text
              ^? key "data"
                . key "result"
                . values
                . key "metric"
                . filtered (F.has $ key "channel" . only "nixpkgs-unstable")
                . key "revision"
                . _String
       in TIO.putStrLn rev
