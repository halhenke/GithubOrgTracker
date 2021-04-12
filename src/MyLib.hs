{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}


module MyLib
  ( module X
  , runOrgs
  )
where


-- import           Control.Monad.IO.Class         ( liftIO )
-- import           Database.Persist
-- import           Database.Persist.Sqlite
-- import           Database.Persist.TH
import           Relude
import           DB.SeldaRepo                  as X
import           GraphQL.API                   as X
import           Data.Time.Clock
import           Database.Selda
import           Database.Selda.SQLite


runOrgs :: [Text] -> IO ()
runOrgs orgs = do
  results <- mapM runRepo orgs
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
updateDB (orgName, dt, rqs) = do
  let o = Org orgName dt
  insert org [o]
  mapM_ updateDBRepos rqs
  -- return ()
  -- let org = orgRef2

updateDBRepos :: MonadSelda m => RepoQuery -> m ()
updateDBRepos rq = do
  let n     = repoQueryName rq
      o     = orgRef2 rq
      lr    = lastRun rq
      ca    = created rq
      repo_ = Repo n o lr ca
  insert repo      [repo_]
  insert repoQuery [rq]
  return ()


-- -- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- --   User
-- --     name String
-- --     age Int
-- --   deriving Show
-- -- |]


-- -- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- -- Person
-- --     name String
-- --     age Int Maybe
-- --     deriving Show
-- -- BlogPost
-- --     title String
-- --     authorId PersonId
-- --     deriving Show
-- -- |]

-- -- share [ mkPersist sqlSettings ] [persistLowerCase|

-- -- Profile
-- --     Id      UserId
-- --     email   String

-- -- |]

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Person
--     firstName String
--     lastName String
--     age Int
--     PersonName firstName lastName
--     deriving Show
-- |]

-- main :: IO ()
-- main = runSqlite ":memory:" $ do
--   runMigration migrateAll
--   insert $ Person "Michael" "Snoyman" 26
--   michael <- getBy $ PersonName "Michael" "Snoyman"
--   liftIO $ print michael

someFunc :: IO ()
someFunc = putStrLn "Fuck"
