{-# OPTIONS_HADDOCK ignore-exports #-}

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


module DB.UpdateDB
  ( runOrgs
  , runOrgsSeq
  )
where



import           Relude
import           DB.SeldaRepo                  as X
import           GraphQL.API                   as X
import           Defaults                      as X
import           Data.Time.Clock
import           Data.Text                     as DT
                                                ( toLower )
import           Colourista.IO                 as CIO
import           Database.Selda                 ( (.==)
                                                , insert
                                                , upsert
                                                , (!)
                                                , (.&&)
                                                , literal
                                                , with
                                                , Col(..)
                                                , MonadMask
                                                , MonadSelda
                                                , Assignment((:=))
                                                )
import           Database.Selda.SQLite

-- | Take a List of 'Org's, query the Github API to get the info on
-- Repositories, and insert it into the SQLite Database
-- NOTE: tis currently runs all Orgs as Queries and only then
-- passes that list of Repos to the Database for storage - I think
-- it should put each `Org` in the Database at a time?
runOrgs :: [Text] -> IO ()
runOrgs orgs = do
  let parsedOrgs = toLower <$> orgs
  results <- mapM runOrg parsedOrgs
  withSQLite githubOrgDb $ mapM updateDB (rights results)
  return ()

-- | Exactly like 'runOrgs', but it fetches an 'Org' from Github and
-- then inserts its 'Repo's into the Database instead of doing all the Github queries
-- for every 'Org' first and then inserting them all into the Database at once.
runOrgsSeq :: [Text] -> IO ()
runOrgsSeq orgs = do
  let parsedOrgs = toLower <$> orgs
  mapM_ runAnOrg parsedOrgs

runAnOrg :: Text -> IO ()
runAnOrg org = do
  (Right repos) <- runOrg org
  withSQLite githubOrgDb $ updateDB repos


-- | updateDB takes an 'Org' Name, a 'UTCTime' and a 'RepoQuery' and
-- it upserts the 'Org' and then maps the 'RepoQuery' over up a
updateDB
  :: (MonadMask m, MonadSelda m, Foldable t)
  => (Text, UTCTime, t RepoQuery)
  -> m ()
updateDB (orgName, dt, rqs) = do
  let o = Org orgName dt
  result <- upsert
    org
    (\orgArg -> orgArg ! #orgName .== (fromString $ toString orgName))
    (\orgArg -> with orgArg [#lastRun := (literal dt)])
    [o]
  case result of
    Just id -> liftIO $ CIO.infoMessage "New Org inserted"
    Nothing -> liftIO $ CIO.infoMessage $ "Update performed on Org " <> orgName
  mapM_ updateDBRepos rqs

-- | 'updateDBRepos' takes a 'RepoQuery' and upserts a 'Repo' into the SQLite
-- Database if it does not already exist before inserting the 'RepoQuery'
updateDBRepos :: (MonadSelda m, MonadMask m) => RepoQuery -> m ()
updateDBRepos rq = do
  let n     = #repoName rq
      o     = #orgRef rq
      lr    = #lastRun rq
      ca    = #createdAt rq
      repo_ = Repo n o lr ca
  result <- upsert
    repo
    (\r -> (r ! #repoName .== (literal n)) .&& (r ! #orgRef .== (literal o)))
    (\r -> with r [#lastRun := (literal lr)])
    [repo_]
  num <- insert repoQuery [rq]
  return ()
