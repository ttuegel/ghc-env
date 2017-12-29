{-

   Copyright 2017 Thomas Tuegel

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}


module Main where

import Control.Applicative
import Control.Monad ((=<<))
import qualified Data.Attoparsec.Text as Parse
import qualified Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Formatting
import Nix.Derivation (Derivation)
import qualified Nix.Derivation as Drv
import Path (Path, Abs, File, Rel, (</>), relfile)
import qualified Path as Path
import qualified Path.IO as Path
import System.Exit (ExitCode(..), exitWith)
import System.IO (IO)
import qualified System.IO.Streams as Stream

import Environment
import qualified Process


main :: IO ()
main =
  do
    nixPath <- getSearchPath "NIX_PATH"
    setSearchPath "NIX_PATH" ("ghc-env=make-ghc-env.nix" : nixPath)
    exprFile <- (</> [relfile|ghc.environment.nix|]) <$> Path.getCurrentDir
    drvFile <- instantiate exprFile
    drv <- parseDrv drvFile
    name <- drvName drvFile drv
    envPath <- Path.parseRelFile ('.' : Text.unpack name)
    realize drvFile envPath
    Path.removeFile drvFile
    pure ()


instantiate :: Path Abs File -> IO (Path Abs File)
instantiate exprFile =
  do
    let
      drvFile = Path.parent exprFile </> [relfile|.ghc.environment.drv|]
      args =
        [
          sformat fp exprFile,
          "--add-root", sformat fp drvFile, "--indirect"
        ]
    () <- Process.run_ [relfile|nix-instantiate|] args
    pure drvFile


realize :: Path Abs File -> Path Rel File -> IO ()
realize drvPath envPath =
  do
    let
      args =
          [
            "--realize", sformat fp drvPath,
            "--add-root", sformat fp envPath, "--indirect"
          ]
    Process.run_ [relfile|nix-store|] args


die :: Text -> IO a
die msg =
  do
    Stream.write (Just msg) =<< Stream.encodeUtf8 Stream.stderr
    exitWith (ExitFailure 1)


parseDrv :: Path Abs File -> IO Derivation
parseDrv drvPath =
    Stream.withFileAsInput (Path.toFilePath drvPath) parseDrv1
  where
    parser = Drv.parseDerivation <* Parse.endOfInput
    parseDrv1 _str =
      do
        _str <- Stream.decodeUtf8 _str
        let get = fromMaybe Text.empty <$> Stream.read _str
        parsed <- Parse.parseWith get parser =<< get
        case parsed of
          Parse.Fail _ _ (Text.pack -> parseErr) ->
            die (sformat (fp%": parse failed: "%s) drvPath parseErr)
          Parse.Partial _ ->
            die "impossible: parser returned partial result"
          Parse.Done _ drv ->
            pure drv


s :: Format r (Text -> r)
s = stext


fp :: Format r (Path a f -> r)
fp = (\render path -> render (Path.toFilePath path)) <$> string


drvName :: Path Abs File -> Derivation -> IO Text
drvName drvPath drv =
    case Map.lookup "name" (Drv.env drv) of
      Nothing ->
        die (sformat (fp%": derivation name not set") drvPath)
      Just name ->
        pure name
