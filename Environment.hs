{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}


module Environment where

import Control.Applicative
import Data.Eq
import Data.Function ((.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified System.Environment as System
import System.IO (IO)


getEnv :: Text -> IO (Maybe Text)
getEnv var = ((<$>) Text.pack) <$> System.lookupEnv (Text.unpack var)

setEnv :: Text -> Text -> IO ()
setEnv var val = System.setEnv (Text.unpack var) (Text.unpack val)


getSearchPath :: Text -> IO [Text]
getSearchPath var = Text.split (== ':') . fromMaybe Text.empty <$> getEnv var


setSearchPath :: Text -> [Text] -> IO ()
setSearchPath var val = setEnv var (Text.intercalate ":" val)
