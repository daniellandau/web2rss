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
import Data.Text as T (Text, pack)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (runStderrLoggingT)

fetch :: String -> IO B.ByteString
fetch url = do
  response <- simpleHttp url
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

save :: Page -> IO ()
save page = runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO $ runSqlConn (insert page) connection >> return ()


migration :: IO ()
migration =
  runStderrLoggingT $ withMySQLConn defaultConnectInfo $ \connection ->
  liftIO (runSqlConn (runMigration migrateAll) connection)


someFunc :: IO String
someFunc = do
  content <- fetch "https://landau.fi"
  now <- getCurrentTime
  save (Page (T.pack "https://landau.fi") content now)
  return . show $ content
