module Main where

-- import qualified MyLib                          ( someFunc )
import           MyLib                         as GHT

main :: IO ()
main = do
  GHT.mkDB
-- main = do
--   putStrLn "Hello, Haskell!"
--   MyLib.someFunc
