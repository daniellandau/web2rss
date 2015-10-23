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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}


module Lib
    ( makeFeed,
      migration
    ) where

import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Text.StringLike
import Data.Maybe
import Database.Persist.Quasi
import Database.Persist.MySQL
import Database.Persist.TH
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format
import System.Locale
import Data.Text as T (Text, pack, unpack)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Text.Feed.Constructor
import Text.Feed.Export
import Text.XML.Light.Output
import Text.Feed.Types
import Data.UUID.V4 as V4
import Data.UUID
import Data.List (sort, find)
import qualified Text.Atom.Feed as AFeed
import qualified Text.Feed.Types as FTypes
import Network.HTTP.Types.Header
import Crypto.Random (getSystemDRG, randomBytesGenerate)
import Crypto.Hash (Digest, MD5, hash)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (MonadLogger)

fetch :: Text -> IO B.ByteString
fetch url = do
  request <- parseUrl $ T.unpack url
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let headers = responseHeaders response
  let contentType = maybe "text/html" snd $ find ((== hContentType ) . fst) headers
  let body = L.toStrict $ responseBody response
  return $ if contentType == "text/html"
     then let tags = parseTags body
              bodyTag = getBody tags
              texts = filter (tagText $ \t -> B.length t > 2) bodyTag
          in innerText texts
     else body

getBody :: StringLike str => [Tag str] -> [Tag str]
getBody tags = dropWhile (~/= body) tags
  where body :: String
        body = "<body>"

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
  $(persistFileWith lowerCaseSettings "models")

getSaved :: (MonadBaseControl IO m, MonadIO m) => Text -> SqlPersistT m [Page]
getSaved url = do
    pageEntities <- selectList [PageUrl ==. url] [Desc PageFetched]
    return $ fmap (\(Entity _ page) -> page) pageEntities

save :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Page -> SqlPersistT m ()
save page = insert page >> return ()

format :: UTCTime -> String
format = formatTime defaultTimeLocale rfc822DateFormat

makeItem :: Text -> UTCTime -> Text -> Item
makeItem url when itemId =
  withItemPubDate (format when)
  . withItemTitle (T.unpack url ++ " has changed")
  . withItemId False ("uurn:uuid:" ++ (T.unpack itemId)) $ newItem AtomKind

prettyPrintFeed :: Feed -> String
prettyPrintFeed = ppElement . xmlFeed

itemsForUrl :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Text -> SqlPersistT m [Item]
itemsForUrl url = do
  content <- liftIO $ fetch url
  now <- liftIO $ getCurrentTime
  saved <- getSaved url
  let latestSaved = listToMaybe saved
  let oldItems = map (\page -> makeItem url (pageFetched page) (pageUuid page)) saved
  let isSame = maybe False (== content) (fmap pageContent latestSaved)
  if isSame
    then return oldItems
    else do
      itemId <- liftIO $ V4.nextRandom
      save (Page url content now (toText itemId))
      return (makeItem url now (toText itemId) : oldItems)

getRandomHash :: IO Text
getRandomHash = do
  drg <- getSystemDRG
  let (bytes, _) = randomBytesGenerate 16 drg
  let byteString = bytes :: B.ByteString
  let digest = hash byteString :: Digest MD5
  return . T.pack . show $ digest

getFeedInfo :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => SqlPersistT m FeedInfo
getFeedInfo = do
    feedEntity <- selectFirst [] []
    let feedInfo = fmap (\(Entity _ feed) -> feed) feedEntity
    if isJust feedInfo
      then return $ fromJust feedInfo
      else do
        newUuid <- fmap toText $ liftIO V4.nextRandom
        newHash <- liftIO getRandomHash
        let newFeedInfo = FeedInfo newUuid newHash
        _ <- insert newFeedInfo
        return newFeedInfo


makeFeed' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => [Text] -> SqlPersistT m String
makeFeed' urls  = do
  items <- mapM itemsForUrl urls >>= return . concat
  feedInfo <- getFeedInfo
  let feed =
        withFeedItems items $ feedFromAtom $
        AFeed.nullFeed
          ("uurn:uuid:" ++ T.unpack (feedInfoUuid feedInfo))
          (AFeed.TextString "Changes in the followed pages")
          (head . reverse . sort . (map (\(FTypes.AtomItem entry) -> AFeed.entryUpdated entry)) $ items)
  return (prettyPrintFeed feed)

makeFeed :: ConnectInfo -> [Text] -> IO String
makeFeed myConnectInfo urls = runStderrLoggingT $ withMySQLConn myConnectInfo $ runSqlConn (makeFeed' urls)

migration :: ConnectInfo -> IO ()
migration myConnectInfo =
  runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
  runSqlConn (runMigration migrateAll) connection
