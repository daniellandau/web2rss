module Main where

import Lib
import Database.Persist.MySQL

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo { connectUser = "web2rss", connectDatabase = "web2rss" }

main :: IO ()
main = migration myConnectInfo >> someFunc myConnectInfo >>= putStrLn
