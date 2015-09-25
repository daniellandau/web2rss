-- Web2RSS - A feed generator, that keeps tabs on web sites
-- Copyright (C) 2015 Daniel Landau
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Lib
import Database.Persist.MySQL
import Data.Text (Text, pack)
import Control.Applicative ((<$>))
import System.Environment
import Yesod

data Web2Rss = Web2Rss
    { connectInfo :: ConnectInfo
    , urls :: [Text]
    }

mkYesod "Web2Rss" [parseRoutes|
/ FeedR GET
|]

instance Yesod Web2Rss

mimeType :: ContentType
mimeType = "application/atom+xml"

getFeedR :: Handler TypedContent
getFeedR = do
  settings <- getYesod
  feedText <- liftIO $ makeFeed (connectInfo settings) (urls settings)
  return $ TypedContent mimeType $ toContent feedText

main :: IO ()
main = do
  user <- lookupEnv "USER"
  db <- lookupEnv "DB"
  password <- lookupEnv "PASSWORD"
  urls <- fmap ((map pack) . words) $ getEnv "URLS"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  let connectInfo = defaultConnectInfo { connectUser = maybe "web2rss" id user
                                       , connectPassword = maybe "" id password
                                       , connectDatabase = maybe "web2rss" id db
                                       }
  migration connectInfo
  warp port $ Web2Rss connectInfo urls
