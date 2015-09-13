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
import Database.Persist.Quasi
import Database.Persist.MySQL
import Database.Persist.TH
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Text as T (Text, pack, unpack)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (runStderrLoggingT)

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

getLatest :: Text -> IO (Maybe B.ByteString)
getLatest url = runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO $ flip runSqlConn connection $ do
    pageEntity <- selectFirst [PageUrl ==. url] [Desc PageFetched]
    return $ fmap (\(Entity _ page) -> pageContent page) pageEntity


save :: Page -> IO ()
save page = runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO $ runSqlConn (insert page) connection >> return ()

migration :: IO ()
migration =
  runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO (runSqlConn (runMigration migrateAll) connection)


someFunc :: IO String
someFunc = do
  let url = "https://landau.fi"
  content <- fetch url
  now <- getCurrentTime
  latestSaved <- getLatest url
  let isSame = maybe False (== content) latestSaved
  if isSame
    then return "They are the same"
    else save (Page url content now) >> return "They differ"
