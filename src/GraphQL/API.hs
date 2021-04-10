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
  -- ( runRepo
  -- )
                   where

import           Data.Morpheus.Client          as DMC
-- import           Relude
import           Relude                  hiding ( ByteString )
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
ghToken = "***REMOVED***"

resolver :: String -> ByteString -> IO ByteString
resolver tok b = runReq defaultHttpConfig $ do
  let headers =
        header "Content-Type" "application/json"
          <> header "User-Agent" "halhenke"
          <> oAuth2Token ghToken
        --   <> header "Authorization"
        --             "Bearer ***REMOVED***"
  (responseBody <$> req POST ghAPI (ReqBodyLbs b) lbsResponse headers)



fetchRepo :: IO (Either String GetRepo)
fetchRepo = fetch (resolver "fake") args
  where args = GetRepoArgs { org = "Google" }


runRepo :: IO ()
runRepo = do
--   (Right repo) <- fetchRepo
  result <- fetchRepo
  case result of
    (Left err) -> print err
    -- (Right repo) -> print (decodeResponse repo)
    (Right (decodeResponse -> Right org)) -> print (parse org)
    otherwise -> print "Seems like some deep Parsing Failed"
--   print repo
  return ()


-- decodeResponse :: GetRepo -> Either Text OrganizationOrganization
decodeResponse (GetRepo _ (Just org)) = Right org
decodeResponse (GetRepo _ _         ) = Left "Something went wrong"

parseRepo :: OrganizationRepositoriesEdgesNodeRepository -> Text
parseRepo o = o ^. _name

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

-- instance IsLabel "name" (OrganizationRepositoriesEdgesNodeRepository -> Text) where
--   fromLabel = name @OrganizationRepositoriesEdgesNodeRepository
--     OrganizationRepositoriesEdgesNodeRepository

-- instance HasField "name" OrganizationRepositoriesEdgesNodeRepository a => IsLabel "name" (OrganizationRepositoriesEdgesNodeRepository -> a) where
--   fromLabel = getField

-- parse :: OrganizationOrganization -> [OrganizationRepositoriesEdgesRepositoryEdge]
-- parse :: OrganizationOrganization -> [OrganizationRepositoriesEdgesNodeRepository]
-- parse :: OrganizationOrganization -> [Text]
-- parse :: OrganizationOrganization -> [(Text, Text, Text, Int)]
parse oo = mapper cc -- & b
 where
  cc =
    oo
      ^. _repositories
      ^. _edges
      &  (^?! _Just)
      &  ((^.. each . _Just . _node . _Just
            -- . (_name <> (_updatedAt . (to getDateTime)))
                                           )
          --  (<| [])
                                            )
  mapper = map
    (\x -> Result {
        -- NOTE: This is how you disambiguate a Record Field Witlh OverloadedLabels
        -- (getField @"name" @OrganizationRepositoriesEdgesNodeRepository) x
                                --  (name x)
                    _repoName  = x ^. _name
                  , _updatedAt = (getDateTime . updatedAt) x
                  , _createdAt = (getDateTime . createdAt) x
                  , _stars     = (#totalCount . stargazers) x
                  , _languages = []
                  , _topics    = []
                  }
    )
  -- mapper = map
  --   (\x ->
  --     ( x ^. _name
  --     , (getDateTime . updatedAt) x
  --     , (getDateTime . createdAt) x
  --     , (totalCount . stargazers) x
  --     )
  --   )
  -- a  = _aaa b
  -- b = (^. _node) d
  -- b  = (^.. each . _node) & d
  -- d  = (^.. each . _Just) -- & e
  -- d = (^?! _Just) (^. _name)
  -- e  = (^.. each . _name)

  -- b  = (^.. _name)
-- ^. _1 ^
-- parseOrg :: OrganizationOrganization ->
