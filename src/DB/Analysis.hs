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


module DB.Analysis
  ( newReposForOrg
  )
where

import           Relude
import           DB.SeldaRepo                  as X
import           GraphQL.API                   as X
import           Defaults                      as X
import           Data.Time.Clock
import           Database.Selda
-- import           Database.Selda                 ( (.==)
--                                                 -- , (:*:)
--                                                 , from
--                                                 , insert
--                                                 , select
--                                                 , query
--                                                 , groupBy
--                                                 , count
--                                                 , (:*:)
--                                                 , upsert
--                                                 , descending
--                                                 , (!)
--                                                 , (.&&)
--                                                 , literal
--                                                 , with
--                                                 , Col(..)
--                                                 , MonadMask
--                                                 , MonadSelda
--                                                 , Assignment((:=))
--                                                 , order
--                                                 )
import           Database.Selda.SQLite

newReposForOrg :: Text -> IO ()
newReposForOrg orgName = do
--   let q = select repoQuery
  -- let q = #repoQueryName `from` select repoQuery
  let q =
        (do
          rq <- select repoQuery
          order (rq ! #lastRun) descending
          return (rq ! #repoName)
        )
  withSQLite github_org_db $ do
    rqn <- query q
    print rqn
    return ()

--

-- aggQ :: (Columns (AggrCols a), Aggregates a) => Query s (AggrCols a)
-- aggQ = do

-- WITH rows as (SELECT repoQueryName, ROW_NUMBER() OVER (PARTITION BY repoQueryName ORDER BY rq.`lastRun`) AS row FROM repoQuery as rq)
-- SELECT `repoQueryName`, COUNT(row) as rc
-- FROM rows as r
-- GROUP BY r.repoQueryName;

-- count_updates

-- repos_per_org
-- -- reposPerOrg :: Text -> IO ()
-- reposPerOrg = do
--   let q = aggregate $ do
--         r <- select repo
--         g <- groupBy (r ! #orgRef)
--         return (g :*: count (r))
--   withSQLite github_org_db $ do
--     result <- query q
--     print result
--     return ()



-- -- Find the number of people living on every address, for all addresses
-- -- with more than one tenant:
-- -- SELECT COUNT(name) AS c, address FROM housing GROUP BY name HAVING c > 1

-- numPpl = do
--   (num_tenants :*: theAddress) <- aggregate $ do
--     h <- select housing
--     theAddress <- groupBy (h ! address)
--     return (count (h ! address) :*: theAddress)
--  restrict (num_tenants .> 1)
--  return (num_tenants :*: theAddress)
