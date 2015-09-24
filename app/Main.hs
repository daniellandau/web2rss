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
  feedText <- liftIO $ someFunc (connectInfo settings) (urls settings)
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
