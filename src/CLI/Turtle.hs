{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}

module CLI.Turtle where


import           Relude
import           Turtle

data Command = IncreaseVolume Int | DecreaseVolume Int deriving (Show)

parser :: Parser Command
parser =
  fmap
      IncreaseVolume
      (subcommand "up"
                  "Turn the volume up"
                  (argInt "amount" "How much to increase the volume")
      )
    <|> fmap
          DecreaseVolume
          (subcommand "down"
                      "Turn the volume down"
                      (argInt "amount" "How much to decrease the volume")
          )

turtlePrompt :: IO ()
turtlePrompt = do
  x <- options "Volume adjuster" parser
  case x of
    IncreaseVolume n -> printf ("Increasing the volume by " % d % "\n") n
    DecreaseVolume n -> printf ("Decreasing the volume by " % d % "\n") n
