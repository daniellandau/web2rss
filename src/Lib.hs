-- {-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.ByteString.Lazy as L
import Text.StringLike
-- import Data.Char

someFunc :: IO String
someFunc = fetch "https://landau.fi"


fetch :: String -> IO String
fetch url = do
  foo <- simpleHttp url
  let tags = parseTags foo
  let body = getBody tags
  let texts = filter (tagText $ \t -> L.length t > 2) body
  return $ show texts
  -- return $ map (chr . fromEnum) (L.unpack foo)

getBody :: StringLike str => [Tag str] -> [Tag str]
getBody tags = dropWhile (~/= "<body>") tags
