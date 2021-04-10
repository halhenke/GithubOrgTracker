{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, OverloadedStrings, OverloadedLabels #-}

module DB.SeldaRepo where

import           Database.Selda
import           Database.Selda.SQLite
import           Relude

data Org = Org {
    orgName :: Text,
    lastRunOrg :: UTCTime
} deriving Generic
instance SqlRow Org

data Repo = Repo {
    repoName :: Text,
    orgRef :: Text,
    lastRunRepo :: UTCTime,
    createdAt :: UTCTime
} deriving Generic
instance SqlRow Repo

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

github_org_db = "haskell-git-org.sqlite"

mkDB :: IO ()
mkDB = withSQLite github_org_db $ do
  createTable org
  createTable repo
  createTable repoQuery
