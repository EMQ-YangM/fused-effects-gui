module Main where

import qualified Example as E
import qualified MyLib as L
import qualified Widget as W

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  E.main
