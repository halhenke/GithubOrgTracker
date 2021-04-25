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

module CLI.Commander where


import           Relude
import           Options.Commander
import           Data.Char                      ( toUpper )



commandPrompt :: IO ()
commandPrompt =
  command_
    .   toplevel @"GitHubTracker"
    .   description @"Here is my GitHubTracker"
    $   (optDef @"m" @"mode" "Hooo" $ \mode -> raw (print $ (mode :: String)))
    <+> (sub @"update" $ raw (print "Hey"))
    <+> (sub @"query" $ raw (print "Ney")) where
    -- <+> (sub @"query" $ raw (print "Ney")) where
    --     defaultProgram =
    --         annotated @"This is the Default Program"
    --         raw $ (print "Ho")


example :: IO ()
example =
  command_
    .   toplevel @"file"
    $   (sub @"maybe-read" $ arg @"filename" \filename -> flag @"read" \b ->
          raw $ if b then putStrLn =<< readFile filename else pure ()
        )
    <+> (sub @"maybe-write" $ opt @"file" @"file-to-write" \mfilename ->
          raw $ case mfilename of
            Just filename -> putStrLn =<< readFile filename
            Nothing       -> pure ()
        )


exampleDeux :: IO ()
exampleDeux =
  command_
    . toplevel @"argument-taker"
    . optDef @"m" @"mode" "Print"
    $ \mode -> arg @"example-argument" $ \arg -> flag @"loud" $ \loud ->
        description
          @"Takes the argument and prints it or not, depending on the mode and possibly loudly"
          . raw
          $ do
              let msg = if loud then map toUpper arg <> "!" else arg
              if mode == ("Print" :: String) then putStrLn msg else pure ()
