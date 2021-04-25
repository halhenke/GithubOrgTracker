{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, OverloadedStrings, OverloadedLabels #-}
{-# LANGUAGE Strict #-}

module DB.SeldaRepo where

import           Database.Selda
import           Database.Selda.SQLite
import           Relude
import           Defaults

-- | The Org Data Type is used for Org Rcords
data Org = Org {
    orgName :: Text,
    lastRunOrg :: UTCTime
} deriving Generic
instance SqlRow Org

-- | The Repo Data Type is used for Repo Records

data Repo = Repo {
    repoName :: Text,
    orgRef :: Text,
    lastRunRepo :: UTCTime,
    createdAt :: UTCTime
} deriving Generic
instance SqlRow Repo

-- | The RepoQuery Data Type is used to represent the result of Github Queries
-- about a particular 'Repo'.
data RepoQuery = RepoQuery {
    repoQueryName :: Text,
    orgRef2 :: Text,
    stars :: Int,
    languages :: Text,
    topics :: Text,
    created :: UTCTime,
    updated :: UTCTime,
    lastRun :: UTCTime
} deriving (Generic, Show, Eq)
instance SqlRow RepoQuery

org :: Table Org
org = table "org" [#orgName :- primary]

repo :: Table Repo
repo = table "repo" [#repoName :- primary, #orgRef :- foreignKey org #orgName]

repoQuery :: Table RepoQuery
repoQuery = table
  "repoQuery"
  [ #repoQueryName :+ #lastRun :- primary
  , #repoQueryName :- foreignKey repo #repoName
  ]


-- | This Creates the 'Org', 'Repo' and 'RepoQuery' Tables if they
-- dont already exist.
mkDB :: IO ()
mkDB = withSQLite github_org_db $ do
  tryCreateTable org
  tryCreateTable repo
  tryCreateTable repoQuery
