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
    -- ( makeFeed,
    --   createFeedInfo,
    --   migration
    -- )
    where

import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import Text.StringLike
import Data.Maybe
import Database.Persist.Quasi
import Database.Persist.MySQL
import Database.Persist.TH
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format
import qualified System.Locale as Locale
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
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput

data Response = Response { body :: B.ByteString, contentType :: B.ByteString }

fetch :: Text -> IO Lib.Response
fetch url = do
  request <- parseUrl $ T.unpack url
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  let headers = responseHeaders response
  let contentType = maybe "text/html" snd $ find ((== hContentType ) . fst) headers
  let body = L.toStrict $ responseBody response
  return $ Response body contentType


parse :: Lib.Response -> B.ByteString
parse response =
  if B.isPrefixOf "text/html" (contentType response)
  then let tags = parseTags (body response)
           bodyTag = getBody tags
           withoutScripts = filterScripts bodyTag
           texts = filter isTagText withoutScripts
       in innerText texts
  else body response


getBody :: [Tag B.ByteString] -> [Tag B.ByteString]
getBody tags = dropWhile (~/= body) tags
  where body :: String
        body = "<body>"

filterScripts :: StringLike str => [Tag str] -> [Tag str]
filterScripts tags = reverse $ filterScripts' False [] tags

filterScripts' :: StringLike str => Bool -> [Tag str] -> [Tag str] -> [Tag str]
filterScripts' _ result [] = result
filterScripts' inScript result (tag : tags)
  | inScript && not (isTagCloseName scriptTagName tag) =
    filterScripts' True result tags
  | inScript && isTagCloseName scriptTagName tag =
    filterScripts' False result tags
  | not inScript && (isTagOpenName scriptTagName tag) =
    filterScripts' True result tags
  | otherwise =
    filterScripts' False (tag : result) tags
  where scriptTagName = Text.StringLike.fromString "script"

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
  $(persistFileWith lowerCaseSettings "models")

getSaved :: (MonadBaseControl IO m, MonadIO m) => Text -> SqlPersistT m [Page]
getSaved url = do
    pageEntities <- selectList [PageUrl ==. url] [Desc PageFetched]
    return $ map entityVal pageEntities

format :: UTCTime -> String
format = formatTime defaultTimeLocale rfc822DateFormat

makeItem :: Text -> UTCTime -> Text -> String -> Item
makeItem url when itemId content = atomEntryToItem $
  item { AFeed.entryContent = Just (AFeed.TextContent content), AFeed.entryLinks = [AFeed.nullLink (T.unpack url)] }
  where item = AFeed.nullEntry ("uurn:uuid:" ++ (T.unpack itemId)) (AFeed.TextString (T.unpack url ++ " has changed")) (format when)

prettyPrintFeed :: Feed -> String
prettyPrintFeed = ppElement . xmlFeed


prettyPrintDiff :: B.ByteString -> B.ByteString -> String
prettyPrintDiff old new =
  ppDiff diffs
  where diffs :: [Diff [String]]
        diffs = getGroupedDiff oldLines newLines
        oldLines = lines $ Char8.unpack old
        newLines = lines $ Char8.unpack new

parseFromSaved :: Page -> B.ByteString
parseFromSaved page = Lib.parse . responseFromPage $ page

hasFeed myConnectInfo feedHash = do
  runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
    runSqlConn (hasFeed' feedHash) connection

hasFeed' feedHash = do
  feedEntityMaybe <- getFeedInfo feedHash
  return $ isJust feedEntityMaybe

addUrlToFeed myConnectInfo feedHash url = do
  runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
    runSqlConn (addUrlToFeed' feedHash url) connection

addUrlToFeed' feedHash url = do
  feedEntityMaybe <- getFeedInfo feedHash
  let feedIdMaybe = fmap (\(Entity key _) -> key) feedEntityMaybe
  if isJust feedIdMaybe
    then let id = fromJust feedIdMaybe
         in insert_ (Url id url)
    else return ()


deleteUrlFromFeed myConnectInfo feedHash urlId = do
  runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
    runSqlConn (deleteUrlFromFeed' feedHash urlId) connection

deleteUrlFromFeed' feedHash urlId = do
  feedEntityMaybe <- getFeedInfo feedHash
  let feedIdMaybe = fmap (\(Entity key _) -> key) feedEntityMaybe
  if isJust feedIdMaybe
     then let id = fromJust feedIdMaybe
          in deleteWhere [UrlFeedId ==. id, UrlId ==. urlId]
    else return ()

responseFromPage page = Lib.Response (pageBody page) (pageContentType page)

itemsForUrl :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => FeedInfoId -> Text -> SqlPersistT m [Item]
itemsForUrl feedId url = do
  response <- liftIO $ fetch url
  let content = Lib.parse response
  now <- liftIO $ getCurrentTime
  saved <- getSaved url
  let latestSaved = listToMaybe saved
  let reverseSaved = reverse saved
  let contents = map parseFromSaved reverseSaved
  let contentPairs = zip ("" : contents) contents
  let diffs = map (\(_1, _2) -> prettyPrintDiff _1 _2) contentPairs
  let oldItems = reverse $ map (\(page, diff) -> makeItem url (pageFetched page) (pageUuid page) diff) (zip reverseSaved diffs)
  let isSame = maybe False (== content) (fmap parseFromSaved latestSaved)
  if isSame
    then return oldItems
    else do
      itemId <- liftIO $ V4.nextRandom
      let oldContent = maybe "" parseFromSaved latestSaved
      let diffContent = (prettyPrintDiff oldContent content)
      insert_ (Page url (body response) (contentType response) now (toText itemId) feedId)
      return (makeItem url now (toText itemId) diffContent : oldItems)

getRandomHash :: IO Text
getRandomHash = do
  drg <- getSystemDRG
  let (bytes, _) = randomBytesGenerate 16 drg
  let byteString = bytes :: B.ByteString
  let digest = hash byteString :: Digest MD5
  return . T.pack . show $ digest

getFeedInfo :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => Text -> SqlPersistT m (Maybe (Entity FeedInfo))
getFeedInfo feedHash =
    selectFirst [FeedInfoHash ==. feedHash] []

createFeedInfo' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => SqlPersistT m String
createFeedInfo' = do
  newUuid <- fmap toText $ liftIO V4.nextRandom
  newHash <- liftIO getRandomHash
  let newFeedInfo = FeedInfo newUuid newHash
  key <- insert newFeedInfo
  return $ T.unpack newHash

createFeedInfo :: ConnectInfo -> IO String
createFeedInfo myConnectInfo = runStderrLoggingT $ withMySQLConn myConnectInfo $ runSqlConn createFeedInfo'

  -- TODO no urls in settings
makeFeed' :: (MonadBaseControl IO m, MonadIO m, MonadLogger m) => [Text] -> Text -> SqlPersistT m (Maybe String)
makeFeed' urls feedHash = do
  feedEntityMaybe <- getFeedInfo feedHash
  if isJust feedEntityMaybe
    then
    let feedEntity = fromJust feedEntityMaybe
        (Entity key feedInfo) = feedEntity
    in do
      items <- mapM (itemsForUrl key) urls >>= return . concat
      let feed = withFeedItems items $ feedFromAtom $
            AFeed.nullFeed ("uurn:uuid:" ++ T.unpack (feedInfoUuid feedInfo))
            (AFeed.TextString "Changes in the followed pages")
            (head . reverse . sort . (map (\(FTypes.AtomItem entry) -> AFeed.entryUpdated entry)) $ items)
      return $ Just (prettyPrintFeed feed)
   else return Nothing

makeFeed :: ConnectInfo -> [Text] -> Text -> IO (Maybe String)
makeFeed myConnectInfo urls feedHash = runStderrLoggingT $ withMySQLConn myConnectInfo $ runSqlConn (makeFeed' urls feedHash)

migration :: ConnectInfo -> IO ()
migration myConnectInfo =
  runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
  runSqlConn (runMigration migrateAll) connection
