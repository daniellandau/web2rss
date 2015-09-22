{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}


module Lib
    ( someFunc,
      migration
    ) where

import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Text.StringLike
import Data.Char (chr)
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
import Data.List (sort)
import qualified Text.Atom.Feed as AFeed
import qualified Text.Feed.Types as FTypes


fetch :: Text -> IO B.ByteString
fetch url = do
  response <- simpleHttp $ T.unpack url
  let tags = parseTags (L.toStrict response)
  let body = getBody tags
  let texts = filter (tagText $ \t -> B.length t > 2) body
  -- return $ show texts
  -- return $ map ((chr . fromEnum) . (L.unpack)) texts
  return $ innerText texts

getBody :: StringLike str => [Tag str] -> [Tag str]
getBody tags = dropWhile (~/= body) tags
  where body :: String
        body = "<body>"

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll"
      ]
  $(persistFileWith lowerCaseSettings "models")


getSaved :: ConnectInfo -> Text -> IO [Page]
getSaved myConnectInfo url = runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
  liftIO $ flip runSqlConn connection $ do
    pageEntities <- selectList [PageUrl ==. url] [Desc PageFetched]
    return $ fmap (\(Entity _ page) -> page) pageEntities


save :: ConnectInfo -> Page -> IO ()
save myConnectInfo page = runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
  liftIO $ runSqlConn (insert page) connection >> return ()

migration :: ConnectInfo -> IO ()
migration myConnectInfo =
  runStderrLoggingT $ withMySQLConn myConnectInfo $ \connection ->
  liftIO (runSqlConn (runMigration migrateAll) connection)

createFeed :: Feed
createFeed = withFeedTitle "Changes in the followed pages" $ newFeed AtomKind


format :: UTCTime -> String
format = formatTime defaultTimeLocale rfc822DateFormat

makeItem :: Text -> UTCTime -> Text -> Item
makeItem url when id =
  withItemPubDate (format when)
  . withItemTitle (T.unpack url ++ " has changed")
  . withItemId False ("uurn:uuid:" ++ (T.unpack id)) $ newItem AtomKind

prettyPrintFeed :: Feed -> String
prettyPrintFeed = ppElement . xmlFeed

itemsForUrl :: ConnectInfo -> Text -> IO [Item]
itemsForUrl myConnectInfo url = do
  content <- fetch url
  now <- getCurrentTime
  saved <- getSaved myConnectInfo url
  let latestSaved = listToMaybe saved
  let emptyFeed = createFeed
  let oldItems = map (\page -> makeItem url (pageFetched page) (pageUuid page)) saved
  let isSame = maybe False (== content) (fmap pageContent latestSaved)
  if isSame
    then return oldItems
    else do
      id <- V4.nextRandom
      save myConnectInfo (Page url content now (toText id))
      return (makeItem url now (toText id) : oldItems)


someFunc :: ConnectInfo -> [Text] -> IO String
someFunc myConnectInfo urls = do
  items <- mapM (itemsForUrl myConnectInfo) urls >>= return . concat
  let feed = withFeedItems items $ withFeedLastUpdate (head . reverse . sort . (map (\(FTypes.AtomItem entry) -> AFeed.entryUpdated entry)) $ items) createFeed
  return (prettyPrintFeed feed)
