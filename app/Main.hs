module Main where

import qualified MyLib as L

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  L.main
