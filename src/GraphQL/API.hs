{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}


module GraphQL.API
  -- ( runOrg
  -- )
                   where

import           Data.Morpheus.Client          as DMC
-- import           Relude
import           Relude                  hiding ( ByteString )
import           Data.Time.Clock
import           Data.Time.Format.ISO8601
import           Language.Haskell.TH
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Char8         as C8
import           Network.HTTP.Req

-- import           Lens.Micro                    as LM
-- import           Lens.Micro.TH                 as LMTH
import           Control.Lens                  as LM
import           Control.Lens.TH               as LMTH
-- import           GHC.Records                    ( HasField(..) ) -- Since base 4.10.0.0
-- import           GHC.OverloadedLabels           ( IsLabel(..) )
import           GraphQL.HelperTH              as GQL
import           GraphQL.Query                 as GQL
-- import           GHC.IsLabel
import           DB.SeldaRepo                  as DBS
                                                ( RepoQuery(..) )
import System.Environment                     as SE (lookupEnv)
import           GHC.Records

data Result = Result {
  _repoName :: Text,
  _createdAt ::Text,
  _updatedAt :: Text,
  _stars :: Int,
  _languages :: [Text],
  _topics :: [Text]
} deriving (Show, Eq)


ghAPI = https "api.github.com" /: "graphql"


-- | resolver is a Function needed by the Morpheus Library to query the API
resolver :: String -> ByteString -> IO ByteString
resolver tok b = do 
  (Just ghToken) <- SE.lookupEnv "GITHUB_API_TOKEN"
  runReq defaultHttpConfig $ do
    let headers =
          header "Content-Type" "application/json"
            <> header "User-Agent" "halhenke"
            <> oAuth2Token (fromString ghToken)
    responseBody <$> req POST ghAPI (ReqBodyLbs b) lbsResponse headers


-- | fetchOrg runs the query defined in 'GraphQL.Query' and returns the Results
fetchOrg :: Text -> IO (Either String GetRepo)
fetchOrg orgName = fetch (resolver "fake") args
  where args = GetRepoArgs { org = (toString orgName) }


-- | runOrg gets the query results for a given 'Org' and then returns
-- the results as a Tuple of the Organization Name,
-- the Time the Query was run and a list of 'RepoQuery'
runOrg :: Text -> IO (Either String (Text, UTCTime, [RepoQuery]))
runOrg orgName = do
--   (Right repo) <- fetchOrg
  print $ "Fetching Repos for Org " <> orgName <> "..."
  result <- fetchOrg orgName
  dt     <- getCurrentTime
  return
    (do
      results <- result
      repos   <- decodeResponse results
      return (orgName, dt, (parse dt orgName repos))
    )



-- decodeResponse :: GetRepo -> Either Text OrganizationOrganization
decodeResponse :: IsString a => GetRepo -> Either a OrganizationOrganization
decodeResponse (GetRepo _ (Just org)) = Right org
decodeResponse (GetRepo _ _         ) = Left "Something went wrong"


cheapDateReader :: Text -> UTCTime
cheapDateReader dt = fromMaybe defaultDate (iso8601ParseM (toString dt))
  where defaultDate = UTCTime (toEnum 3) (fromInteger 4)

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x


-- | parse takes the results of a Github Query and Parses them into a list of
-- 'RepoQuery's that can be inserted into the Database
parse :: UTCTime -> Text -> OrganizationOrganization -> [RepoQuery]
parse fetchTime orgName oo = mapper cc
 where
  cc =
    oo
      ^. _repositories
      ^. _edges
      &  (^?! _Just)
      &  ((^.. each . _Just . _node . _Just))
  mapper = map
    (\x -> RepoQuery
      {
        -- NOTE: This is how you disambiguate a Record Field Witlh OverloadedLabels
        -- (getField @"name" @OrganizationRepositoriesEdgesNodeRepository) x
                                --  (name x)
        repoName  = x ^. _name
      , orgRef    = orgName
      , createdAt = (cheapDateReader . getDateTime . #createdAt) x
      , updatedAt = (cheapDateReader . getDateTime . #updatedAt) x
      , lastRun   = fetchTime
      , stars     = (#totalCount . stargazers) x
      , languages = fromMaybe "" (langParser x)
      , topics    = fromMaybe "" (topicParser x)
      }
    )
  langParser = \x -> do
    y <- GQL.languages x
    y <- #edges y
    let yyy = catMaybes y
    let y4  = #name . #node <$> yyy
    let y5 = toText $ intercalate ", " (toString <$> y4)
    return y5
  topicParser = \x -> do
    y <- return $ GQL.repositoryTopics x
    y <- #edges y
    let yy  = catMaybes y
    let yyy = #name . #topic <$> (catMaybes $ #node <$> yy)
    let y5 = toText $ intercalate ", " (toString <$> yyy)
    return y5
