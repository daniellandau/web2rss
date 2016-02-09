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
{-# LANGUAGE ViewPatterns      #-}
module Main where

import Lib
import Database.Persist.MySQL
import Data.Text (Text, pack)
import Data.Maybe
import Control.Applicative ((<$>))
import System.Environment
import Yesod

data Web2Rss = Web2Rss
    { connectInfo :: ConnectInfo
    , urls :: [Text]
    , sourceCodeUrl :: Text
    }

mkYesod "Web2Rss" [parseRoutes|
/                     MainR     GET
/feeds/#Text          FeedR     GET
/feeds                FeedsR     POST
|]

instance Yesod Web2Rss

contentTypeAtom :: ContentType
contentTypeAtom = "application/atom+xml"

contentTypeJson :: ContentType
contentTypeJson = "application/json"

getMainR :: Handler Html
getMainR = do
  settings <- getYesod
  let url = sourceCodeUrl settings
  defaultLayout [whamlet|
     <h1>Web2RSS
     <p>This web site is free software under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
     <p>You can find the source code at <a href="#{url}">#{url}</a>.
|]

getFeedR :: Text -> Handler TypedContent
getFeedR hash = do
  settings <- getYesod
  -- TODO no urls in settings
  feedTextMaybe <- liftIO $ makeFeed (connectInfo settings) (urls settings) hash
  if isJust feedTextMaybe
    then return $ TypedContent contentTypeAtom $ toContent $ fromJust feedTextMaybe
    else notFound

postFeedsR :: Handler TypedContent
postFeedsR = do
  settings <- getYesod
  key <- liftIO $ createFeedInfo $ connectInfo settings
  return $ TypedContent contentTypeJson $ toContent ("{'key':'"++key++"'}")

main :: IO ()
main = do
  user <- lookupEnv "USER"
  db <- lookupEnv "DB"
  password <- lookupEnv "PASSWORD"
  -- TODO no urls in settings
  urls <- fmap ((map pack) . words) $ getEnv "URLS"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  sourceCodeUrl <- fmap pack $ maybe "https://github.com/daniellandau/web2rss" id <$> lookupEnv "SOURCE_CODE_URL"
  let connectInfo = defaultConnectInfo { connectUser = maybe "web2rss" id user
                                       , connectPassword = maybe "" id password
                                       , connectDatabase = maybe "web2rss" id db
                                       }
  migration connectInfo
  warp port $ Web2Rss connectInfo urls sourceCodeUrl
