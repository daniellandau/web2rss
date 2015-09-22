{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Lib
import Database.Persist.MySQL
import Data.Text

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo { connectUser = "web2rss", connectDatabase = "web2rss" }

urls :: [Text]
urls = ["http://whatif.xkcd.com", "https://landau.fi"]

main :: IO ()
main = migration myConnectInfo >> someFunc myConnectInfo urls >>= putStrLn
