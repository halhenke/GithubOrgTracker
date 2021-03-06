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


module CLI.Generic
  ( genericPrompt
  )
where

import           Relude                        as R
import           Options.Generic               as OG
import           DB.UpdateDB                   as UDB
import           DB.Analysis                   as A
import           DB.SeldaRepo                  as DBS
import           Colourista                    as C
import           Defaults

-- data Repo = Repo {
--     repos :: [Text]
-- } deriving (Show, Eq, Generic)

-- data Repos = All | Repos Repo
--     deriving (Show, Eq, Generic)

-- data Command =
--     Update Repos
--     | Query
--     deriving (Show, Eq, Generic)

-- myModifiers :: Modifiers
-- myModifiers = defaultModifiers { constructorNameModifier = id }

-- -- instance ParseRecord MyType where
-- --     parseRecord = parseRecordWithModifiers myModifiers

-- instance ParseRecord Repo where
--   parseRecord = parseRecordWithModifiers myModifiers
-- instance ParseRecord Repos where
--   parseRecord = parseRecordWithModifiers myModifiers
-- instance ParseRecord Command where
--   parseRecord = parseRecordWithModifiers myModifiers

-- instance ParseFields Repos where
--     parseFields (Just h)

data Command =
    UpdateAll
    | Update [Text]
    | Query Text
    | MakeTables
    | Migrate
    | DestroyAll
    | Destroy [Text]
    | DescribeAllTables
    | ListOrgs
    deriving (Show, Eq, Generic)

instance ParseRecord Command

parseCommand :: Command -> IO ()
parseCommand (Update orgs)     = runOrgsSeq orgs
parseCommand UpdateAll         = runOrgsSeq defaultOrgs
parseCommand (Query org)       = A.newReposForOrg org
parseCommand MakeTables        = DBS.makeTables
parseCommand Migrate           = DBS.migrateFix
parseCommand DestroyAll        = DBS.destroyTables allTableNams
parseCommand (Destroy tables)  = DBS.destroyTables tables
parseCommand DescribeAllTables = DBS.describeTables allTableNams
parseCommand ListOrgs          = do
  C.formattedMessage [C.blue] " ------"
  C.formattedMessage [C.blue] "| Orgs |"
  C.formattedMessage [C.blue] " ------"
  let orgPrint = C.formattedMessage [C.green]
  mapM_ orgPrint defaultOrgs

-- | This parses Command Line Arguments and runs the appropriate Command
genericPrompt :: IO ()
genericPrompt = do
  x <- getRecord @IO @Command "Test Example"
  print $ "You have Entered " <> show x
  parseCommand x
