{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import Database.Persist.MySQL
import Data.Text (pack)
import System.Environment

main :: IO ()
main = do
  user <- lookupEnv "USER"
  db <- lookupEnv "DB"
  password <- lookupEnv "PASSWORD"
  urls <- fmap ((map pack) . words) $ getEnv "URLS"
  let connectInfo = defaultConnectInfo { connectUser = maybe "web2rss" id user
                                       , connectPassword = maybe "" id password
                                       , connectDatabase = maybe "web2rss" id db
                                       }
  migration connectInfo
  feedText <- someFunc connectInfo urls
  putStrLn feedText
