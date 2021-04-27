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
  )
where



-- import           Control.Monad.IO.Class         ( liftIO )
-- import           Database.Persist
-- import           Database.Persist.Sqlite
-- import           Database.Persist.TH
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
runOrgs :: [Text] -> IO ()
runOrgs orgs = do
  let parsedOrgs = toLower <$> orgs
  results <- mapM runOrg parsedOrgs
  -- out     <- liftIO $ mapM updateDB (rights results)
  withSQLite github_org_db $ mapM updateDB (rights results)
  -- print $ (show @Text) (length results) <> " is the length of the results"
  -- return $ updateDB (results !! 0)
  -- updateDB (head results)
  -- bracket
  -- updateDB results
  return ()

-- updateDB :: MonadSelda m => (Text, UTCTime, [RepoQuery]) -> m ()
-- updateDB :: MonadSelda m => Text -> UTCTime -> [RepoQuery]) -> m ()
-- updateDB []
-- updateDB rq:rqx

-- | updateDB takes an 'Org' Name, a 'UTCTime' and a 'RepoQuery' and
-- it upserts the 'Org' and then maps the 'RepoQuery' over up a
updateDB
  :: (MonadMask m, MonadSelda m, Foldable t)
  => (Text, UTCTime, t RepoQuery)
  -> m ()
updateDB (orgName, dt, rqs) = do
  let o = Org orgName dt
  -- insert org [o]
  result <- upsert
    org
    (\orgArg -> orgArg ! #orgName .== (fromString $ toString orgName))
    (\orgArg -> with orgArg [#lastRun := (literal dt)])
    [o]
  case result of
    Just id -> liftIO $ CIO.infoMessage "New Org inserted"
    Nothing -> liftIO $ CIO.infoMessage $ "Update performed on Org " <> orgName
  mapM_ updateDBRepos rqs
  -- return ()
  -- let org = orgRef2

-- | 'updateDBRepos' takes a 'RepoQuery' and upserts a 'Repo' into the SQLite
-- Database if it does not already exist before inserting the 'RepoQuery'
updateDBRepos :: (MonadSelda m, MonadMask m) => RepoQuery -> m ()
updateDBRepos rq = do
  let n     = #repoName rq
      o     = #orgRef rq
      lr    = #lastRun rq
      ca    = #createdAt rq
      repo_ = Repo n o lr ca
  -- insert repo [repo_]
  result <- upsert
    repo
    (\r -> (r ! #repoName .== (literal n)) .&& (r ! #orgRef .== (literal o)))
    (\r -> with r [#lastRun := (literal lr)])
    [repo_]
  -- case result of
  --   Just id -> print "New Repo inserted"
  --   Nothing -> print $ "Update performed on Repo " <> n
  num <- insert repoQuery [rq]
  -- print $ "Inserted " <> show num <> " times for Repo " <> n
  return ()
