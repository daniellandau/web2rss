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
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Lib
import MyPrelude
import Data.Aeson.Encode
import Data.Aeson.Types
import Database.Persist.MySQL
import Data.Text (Text, pack)
import Data.Maybe
import Control.Applicative ((<$>))
import System.Environment
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Yesod
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Default.Config2
import Yesod.Static --(staticFiles, Static, base64md5, StaticRoute)
import Text.Julius
import Database.Persist.Class
import qualified Data.Vector as V
import GHC.Int

staticFiles ("static")

data Web2Rss = Web2Rss
    { connectInfo    :: ConnectInfo
    , connectionPool :: ConnectionPool
    , sourceCodeUrl  :: Text
    , staticSettings :: Static
    , appRoot        :: Text
    }


mkYesod "Web2Rss" [parseRoutes|
/                           MainR         GET
/feeds/#Text                FeedR         GET
/feeds/#Text/url            FeedUrlsR     POST GET
/feeds/#Text/url/#UrlId     FeedUrlR      DELETE PUT
/feeds                      FeedsR        POST
/static                     StaticR       Static staticSettings
/urls                       UrlsR         GET
|]

instance Yesod Web2Rss where
  approot = ApprootMaster $ appRoot
  addStaticContent ext mime content = do
    settings <- getYesod
    addStaticContentExternal
      minifym genFileName "static" (StaticR . flip StaticRoute [])
      ext mime content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

contentTypeAtom :: ContentType
contentTypeAtom = "application/atom+xml"

contentTypeJson :: ContentType
contentTypeJson = "application/json"

contentTypeTextPlain :: ContentType
contentTypeTextPlain = "text/plain"

getUrlsR :: Handler Javascript
getUrlsR = do
  render <- getUrlRenderParams
  let js = [julius|
                  var FEEDSURL = _.template(decodeURI("@{FeedUrlsR "<%= feedHash %>"}"));
                  var FEEDURL  = _.template(decodeURI("@{FeedUrlR  "<%= feedHash %>" (toSqlKey 123)}").replace("123", "<%= urlId %>")); |]
  return $ js render

getMainR :: Handler Html
getMainR = do
  settings <- getYesod
  let url = sourceCodeUrl settings
  defaultLayout $ do
    addScript $ StaticR lodash_js
    addScript $ UrlsR
    addScript $ StaticR index_js
    [whamlet|
            <h1>Web2RSS
            <p>This web site is free software under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
            <p>You can find the source code at <a href="#{url}">#{url}</a>.
            <div #app>
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

data JsonUrl = JsonUrl { url :: Text, id :: Int64 }

instance FromJSON JsonUrl where
  parseJSON (Object o) = JsonUrl <$> o .: "url" <*> o .: "id"

instance ToJSON JsonUrl where
  toJSON (JsonUrl url id) = object ["url" .= url, "id" .= id]

run :: (MonadBaseControl IO m, MonadIO m) =>
     Web2Rss -> SqlPersistT (Control.Monad.Logger.LoggingT m) a -> m a
run settings = runStderrLoggingT . (flip runSqlPool $ connectionPool settings)

getFeedUrlsR :: Text -> Handler Value
getFeedUrlsR hash = do
  settings <- getYesod
  urls <- runError $ run settings $ urlsForFeed hash
  return  $ Array . V.fromList $ map toJSON $ map (\(url, key) -> JsonUrl url (fromSqlKey key)) urls


postFeedUrlsR :: Text -> Handler TypedContent
postFeedUrlsR hash = do
  settings <- getYesod
  JsonUrl url _ <- requireJsonBody
  runError $ run settings $ addUrlToFeed hash url
  ok

runError a = do
  js <- runExceptT a
  case js of
    Left msg -> permissionDenied msg
    Right x  -> return x

ok :: Handler TypedContent
ok = return . TypedContent contentTypeTextPlain . toContent . pack $ "ok"

putFeedUrlR :: Text -> UrlId -> Handler TypedContent
putFeedUrlR hash urlId = do
  settings <- getYesod
  JsonUrl url _ <- requireJsonBody :: Handler JsonUrl
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
  appRoot <- maybe "/" pack <$> lookupEnv "APPROOT"
  let connectInfo = defaultConnectInfo { connectUser = maybe "web2rss" identity user
                                       , connectPassword = maybe "" identity password
                                       , connectDatabase = maybe "web2rss" identity db
                                       }
  pool <- runStderrLoggingT $ createMySQLPool connectInfo 2
  staticSettings <- static "static"
  let settings = Web2Rss connectInfo pool sourceCodeUrl staticSettings appRoot
  run settings migration
  warp port $ settings
