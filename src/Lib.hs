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

getSaved :: Text -> IO [Page]
getSaved url = runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO $ flip runSqlConn connection $ do
    pageEntities <- selectList [PageUrl ==. url] [Desc PageFetched]
    return $ fmap (\(Entity _ page) -> page) pageEntities


save :: Page -> IO ()
save page = runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO $ runSqlConn (insert page) connection >> return ()

migration :: IO ()
migration =
  runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO (runSqlConn (runMigration migrateAll) connection)

createFeed :: Feed
createFeed = withFeedTitle "Changes in the followed pages" $ newFeed AtomKind


format :: UTCTime -> String
format = formatTime defaultTimeLocale rfc822DateFormat

makeItem :: Text -> UTCTime -> Item
makeItem url when =
  let item   = newItem AtomKind
      item'  = withItemPubDate (format when) item
      item'' = withItemTitle (T.unpack url ++ " has changed") item'
  in item''

prettyPrintFeed :: Feed -> String
prettyPrintFeed = ppElement . xmlFeed

someFunc :: IO String
someFunc = do
  let url = "https://extensions.gnome.org/extension/973/switcher/"
  content <- fetch url
  now <- getCurrentTime
  saved <- getSaved url
  let latestSaved = listToMaybe saved
  let emptyFeed = createFeed
  let oldItems = map (\page -> makeItem url (pageFetched page)) saved
  let isSame = maybe False (== content) (fmap pageContent latestSaved)
  feed <- if isSame
    then return (withFeedItems oldItems $ withFeedLastUpdate (format (maybe now pageFetched latestSaved)) emptyFeed)
    else save (Page url content now) >> return (withFeedItems (makeItem url now : oldItems) $ withFeedLastUpdate (format now) emptyFeed)
  return (prettyPrintFeed feed)
