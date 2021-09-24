{-# LANGUAGE Strict #-}


module Main where

import           Relude
import           MyLib                         as GHT
import Control.Monad (void)
import Configuration.Dotenv

main :: IO ()
main = do
  void $ loadFile defaultConfig
  GHT.genericPrompt
  -- GHT.commandPrompt
  -- GHT.turtlePrompt
