{-# LANGUAGE Strict #-}


module Main where

import           Relude
-- import qualified MyLib                          ( someFunc )
import           MyLib                         as GHT

main :: IO ()
main = do
  -- GHT.mkDB
  -- GHT.runOrgs ["Google"]
  GHT.newReposForOrg "Google"
-- main = do
--   putStrLn "Hello, Haskell!"
--   MyLib.someFunc
