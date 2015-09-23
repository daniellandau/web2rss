{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import Database.Persist.MySQL
import Data.Text (Text)

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo { connectUser = "web2rss", connectDatabase = "web2rss" }

urls :: [Text]
urls = ["http://whatif.xkcd.com", "https://landau.fi", "https://extensions.gnome.org/comments/all/?pk=973&all=false"]

main :: IO ()
main = do
  migration myConnectInfo
  feedText <- someFunc myConnectInfo urls
  putStrLn feedText
