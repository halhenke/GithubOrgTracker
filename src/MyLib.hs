{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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


runOrgs :: [Text] -> IO ()
runOrgs orgs = do
  results <- mapM runRepo orgs
  print results
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
