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
