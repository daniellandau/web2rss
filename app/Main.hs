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
{-# LANGUAGE FlexibleContexts  #-}
module Main where

import Lib
import MyPrelude
import Data.Aeson.Encode
import Database.Persist.MySQL
import Data.Text (Text, pack)
import Data.Maybe
import Control.Applicative ((<$>))
import System.Environment
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Yesod

data Web2Rss = Web2Rss
    { connectInfo    :: ConnectInfo
    , connectionPool :: ConnectionPool
    , sourceCodeUrl  :: Text
    }

mkYesod "Web2Rss" [parseRoutes|
/                           MainR         GET
/feeds/#Text                FeedR         GET
/feeds/#Text/url            FeedUrlsR     POST
/feeds/#Text/url/#UrlId     FeedUrlR      DELETE PUT
/feeds                      FeedsR        POST
|]

instance Yesod Web2Rss

contentTypeAtom :: ContentType
contentTypeAtom = "application/atom+xml"

contentTypeJson :: ContentType
contentTypeJson = "application/json"

contentTypeTextPlain :: ContentType
contentTypeTextPlain = "text/plain"

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
  feedText <- runError $ run settings $ makeFeed hash
  return $ TypedContent contentTypeAtom $ toContent $ feedText

postFeedsR :: Handler TypedContent
postFeedsR = do
  settings <- getYesod
  key <- run settings createFeedInfo
  return $ TypedContent contentTypeJson $ toContent ("{'key':'"++key++"'}")

data JsonUrl = JsonUrl { url :: Text }

run :: (MonadBaseControl IO m, MonadIO m) =>
     Web2Rss -> SqlPersistT (Control.Monad.Logger.LoggingT m) a -> m a
run settings = runStderrLoggingT . (flip runSqlPool $ connectionPool settings)

instance FromJSON JsonUrl where
  parseJSON (Object o) = JsonUrl <$> o .: "url"

postFeedUrlsR :: Text -> Handler TypedContent
postFeedUrlsR hash = do
  settings <- getYesod
  JsonUrl url <- requireJsonBody
  runError $ run settings $ addUrlToFeed hash url
  ok

runError a = do
  foo <- runExceptT a
  case foo of
    Left msg -> permissionDenied msg
    Right x  -> return x

ok :: Handler TypedContent
ok = return . TypedContent contentTypeTextPlain . toContent . pack $ "ok"

putFeedUrlR :: Text -> UrlId -> Handler TypedContent
putFeedUrlR hash urlId = do
  settings <- getYesod
  JsonUrl url <- requireJsonBody :: Handler JsonUrl
  runError $ run settings $ modifyUrlInFeed hash urlId url
  ok

deleteFeedUrlR :: Text -> UrlId -> Handler TypedContent
deleteFeedUrlR feedHash urlId = do
  settings <- getYesod
  runError $ run settings (deleteUrlFromFeed feedHash urlId)
  ok

main :: IO ()
main = do
  user <- lookupEnv "USER"
  db <- lookupEnv "DB"
  password <- lookupEnv "PASSWORD"
  port <- maybe 3000 read <$> lookupEnv "PORT"
  sourceCodeUrl <- maybe "https://github.com/daniellandau/web2rss" pack <$> lookupEnv "SOURCE_CODE_URL"
  let connectInfo = defaultConnectInfo { connectUser = maybe "web2rss" identity user
                                       , connectPassword = maybe "" identity password
                                       , connectDatabase = maybe "web2rss" identity db
                                       }
  pool <- runStderrLoggingT $ createMySQLPool connectInfo 2
  let settings = Web2Rss connectInfo pool sourceCodeUrl
  run settings migration
  warp port $ settings
