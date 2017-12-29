{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}


module Process where

import Control.Applicative
import Control.Exception (Exception, throw)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.Async as Async
import Control.Monad (Monad(..), (=<<))
import Data.List (map)
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Path (Path, File)
import qualified Path
import System.Exit (ExitCode(..))
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Stream
import System.IO (IO)
import Text.Show (Show)


data ProcessFailed =
    forall a.
    ProcessFailed {
      path :: Path a File,
      args :: [Text],
      exit :: ExitCode
    }

deriving instance Show ProcessFailed
instance Exception ProcessFailed


type Input = InputStream Text
type Output = OutputStream Text


run :: Path a File -> [Text] -> IO (Output, Input, Async ())
run path args =
  do
    (inp, outp, err, ph)
      <-
        Stream.runInteractiveProcess
          (Path.toFilePath path)
          (map Text.unpack args)
          Nothing
          Nothing
    Stream.supply err Stream.stderr
    (,,) <$> encode inp <*> decode outp <*> async (waitFor ph)
  where
    decode s = Stream.lines s >>= Stream.decodeUtf8
    encode s = Stream.unlines s >>= Stream.encodeUtf8
    waitFor ph =
        \case
          ExitSuccess -> pure ()
          exit@(ExitFailure _) -> throw (ProcessFailed {..})
          =<< Stream.waitForProcess ph

run_ :: Path a File -> [Text] -> IO ()
run_ path args =
  do
    (inp, outp, exit) <- run path args
    Stream.write Nothing inp
    Stream.connect outp =<< Stream.nullOutput
    Async.wait exit

inproc :: Path a File -> [Text] -> IO (Input, Async())
inproc path args =
  do
    (inp, outp, exit) <- run path args
    Stream.write Nothing inp
    pure (outp, exit)
