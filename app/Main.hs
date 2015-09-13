module Main where

import Lib

main :: IO ()
main = migration >> someFunc >>= putStrLn
