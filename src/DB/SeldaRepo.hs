{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Strict #-}

module DB.SeldaRepo where

import           Database.Selda                as DBS
import           Database.Selda.SQLite         as DBS
import           Database.Selda.Migrations     as DBS
import           Database.Selda.Backend        as DBS
import           Database.Selda.Unsafe         as DBU
import           Database.Selda.Validation     as DBV
import           Colourista.IO                 as CIO
import           Relude
import           Control.Monad.Catch
import           Defaults

-- | The Org Data Type is used for Org Rcords
data Org = Org {
    orgName :: Text,
    lastRun :: UTCTime
} deriving Generic
instance SqlRow Org

-- | The Repo Data Type is used for Repo Records

data Repo = Repo {
    repoName :: Text,
    orgRef :: Text,
    lastRun :: UTCTime,
    createdAt :: UTCTime
} deriving Generic
instance SqlRow Repo

-- | The RepoQuery Data Type is used to represent the result of Github Queries
-- about a particular 'Repo'.
data RepoQuery = RepoQuery {
    repoName :: Text,
    orgRef :: Text,
    stars :: Int,
    languages :: Text,
    topics :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    lastRun :: UTCTime
} deriving (Generic, Show, Eq)
instance SqlRow RepoQuery

org :: Table Org
org = table "org" [#orgName :- primary]

-- repo :: Table Repo
-- repo = table "repo" [#repoName :- primary, #orgRef :- foreignKey org #orgName]
repo :: Table Repo
repo = table "repo" [#repoName :+ #orgRef :- primary]
  -- , #orgRef :- foreignKey org #orgName]

-- repoQuery :: Table RepoQuery
-- repoQuery = table
--   "repoQuery"
--   [ #repoName :+ #lastRun :- primary
--   , #repoName :- foreignKey repo #repoName
--   ]
repoQuery :: Table RepoQuery
repoQuery = table "repoQuery" [#repoName :+ #orgRef :+ #lastRun :- primary]
  -- , #orgRef :- foreignKey org #orgName
  -- , #repoName :- foreignKey repo #repoName
  -- , #orgRef :- foreignKey repo #orgRef
  -- ]

r2 :: Table Repo
r2 = table
  "repo"
  [#repoName :+ #orgRef :- primary, #orgRef :- foreignKey org #orgName]

rq2 :: Table RepoQuery
rq2 = table
  "repoQuery"
  [ #repoName :+ #orgRef :+ #lastRun :- primary
  , #orgRef :- foreignKey org #orgName
  , #repoName :- foreignKey repo #repoName
  ]

setUpDB :: (MonadSelda m) => m ()
setUpDB = DBU.rawStm "PRAGMA foreign_keys = ON;"

checkDB :: (MonadSelda m) => m ()
checkDB = DBU.rawStm "PRAGMA foreign_keys"

-- | This Creates the 'Org', 'Repo' and 'RepoQuery' Tables if they
-- dont already exist.
makeTables :: IO ()
makeTables = withSQLite github_org_db $ do
  setUpDB
  tryCreateTable org
  tryCreateTable repo
  tryCreateTable repoQuery

migrateFix :: IO ()
migrateFix = withSQLite github_org_db $ do
  let myID x = x
  -- setUpDB
  -- migrate repo      r2  myID
  -- migrate repoQuery rq2 myID
  checkDB

data TableRep = OrgTable | RepoTable | RepoQueryTable

parseTable :: Text -> TableRep
parseTable "org"       = OrgTable
parseTable "repo"      = RepoTable
parseTable "repoQuery" = RepoQueryTable


-- parseTable :: SqlRow a => Text -> _
-- parseTable "org"       = org
-- parseTable "repo"      = repo
-- parseTable "repoQuery" = repoQuery

-- proxyTable :: Text -> Proxy a
-- proxyTable "org" = Proxy @(Table Org)
-- proxyTable "repo"      = repo
-- proxyTable "repoQuery" = repoQuery


-- | delete a list of 'Table's
destroyTables :: [Text] -> IO ()
destroyTables tableNames = withSQLite github_org_db $ do
  -- parseTable <$> tableNames
  -- (tryDropTable . _r) <$> tableNames
  -- let tables = _r <$> tableNames
  -- mapM_ tryDropTable tables
  mapM_ destroyTable tableNames

destroyTable :: (MonadSelda m, MonadThrow m, MonadMask m) => Text -> m ()
destroyTable "org"  = withoutForeignKeyEnforcement $ tryDropTable org
destroyTable "repo" = withoutForeignKeyEnforcement $ tryDropTable repo
destroyTable "repoQuery" =
  withoutForeignKeyEnforcement $ tryDropTable repoQuery
destroyTable t = throwM $ SqlError $ "Table " ++ (show t) ++ " does not exist."

-- checkTables :: [Text] -> IO ()
-- checkTables orgs = do
--   print "Yaah"
--   conn <- sqliteOpen github_org_db
--   withConnection conn mank
--   where mank = describeTable . mkTableName $ "org"

-- connAct :: MonadSelda  m => SeldaConnection SQLite -> m a -> m a
-- connAct conn act = withConnection conn
-- checkTables :: [Text] -> IO ()
-- checkTables orgs = withSQLite github_org_db $ do
--   let tables = (describeTable . mkTableName) :: Text -> IO TableInfo
--   -- tables <- (describeTable . mkTableName)
--   -- let t2     = print tables
--   -- mapM_ t2 orgs
--   liftIO $ print "This is an action"
--   return ()

describeTables :: [Text] -> IO ()
describeTables tables = mapM_ printTableDescription tables

printTableDescription :: Text -> IO ()
printTableDescription t = withSQLite github_org_db $ do
  liftIO $ infoMessage $ "Description of Table " <> t
  ti <- DBV.describeTable . mkTableName $ t
  print ti
--   -- print "Horse"
--   print t
