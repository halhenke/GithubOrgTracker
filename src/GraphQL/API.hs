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
{-# LANGUAGE TypeApplications #-}


module GraphQL.API
  ( runRepo
  )
where

import           Data.Morpheus.Client          as DMC
import           Data.FileEmbed
-- import           Relude
import           Relude                  hiding ( ByteString )
import           GraphQL.HelperTH
import           Language.Haskell.TH
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Char8         as C8
import           Network.HTTP.Req
-- defineByDocumentFile "./src/GraphQL/github.gql"
--     [gql|
--         query GetHero ($character: Character)
--         {
--             deity (fatherOf:$character) {
--             name
--             power
--             worships {
--                 deity2Name: name
--             }
--             }
--         }
--     |]

data HTML = HTML Text
    deriving (Typeable, DecodeScalar, EncodeScalar, Eq, Show)
data DateTime = DateTime Text
    deriving (Typeable, DecodeScalar, EncodeScalar, Eq, Show)


-- githubApi :: ByteString -> IO ByteString
-- githubApi req = do
--   print req
--   return "bad"

ghAPI = "https://api.github.com/graphql"
ghToken = "***REMOVED***"

resolver :: String -> ByteString -> IO ByteString
resolver tok b = runReq defaultHttpConfig $ do
  let headers =
        header "Accept" "application/json"
          <> header "User-Agent" "halhenke"
          <> oAuth2Token ghToken
        --   <> header "Authorization"
        --             "Bearer ***REMOVED***"
  (responseBody <$> req POST (https ghAPI) (ReqBodyLbs b) lbsResponse headers)
  (responseBody <$> req GET (https ghAPI) (ReqBodyLbs b) lbsResponse headers)



defineByDocumentFile'
    (makeRelativeToProject "src/GraphQL/schema.gql")
    [gql|
        query GetRepo($org: String!){
            rateLimit {
                cost
                remaining
                resetAt
            }
            organization(login: $org) {
                repositories(orderBy: {field: PUSHED_AT, direction: DESC}, first: 100) {
                edges {
                    node {
                        name,
                        description,
                        descriptionHTML,
                        shortDescriptionHTML,
                        stargazers(first: 1) {
                            totalCount
                        }
                        createdAt,
                        pushedAt,
                        updatedAt,
                        primaryLanguage {
                            name
                        }
                        languages(first: 10) {
                            edges {
                                node {
                                    name
                                }
                            }
                        }
                        repositoryTopics(first: 10) {
                            edges {
                            node {
                                topic {
                                name
                                }
                            }
                        }
                    }
                    }
                }
                }
            }
        }
    |]


-- queryGithub = do
--   q <-
--     -- (defineByDocumentFile'
--     --   (makeRelativeToProject "src/GraphQL/schema.gql")
--     --   [gql|
--     --         query GetRepo($org: String!){
--     --             rateLimit {
--     --                 cost
--     --                 remaining
--     --                 resetAt
--     --             }
--     --             organization(login: "$org") {
--     --                 repositories(orderBy: {field: PUSHED_AT, direction: DESC}, first: 100) {
--     --                 edges {
--     --                     node {
--     --                         name,
--     --                         description,
--     --                         descriptionHTML,
--     --                         shortDescriptionHTML,
--     --                         stargazers(first: 1) {
--     --                             totalCount
--     --                         }
--     --                         createdAt,
--     --                         pushedAt,
--     --                         updatedAt,
--     --                         primaryLanguage {
--     --                             name
--     --                         }
--     --                         languages(first: 10) {
--     --                             edges {
--     --                                 node {
--     --                                     name
--     --                                 }
--     --                             }
--     --                         }
--     --                         repositoryTopics(first: 10) {
--     --                             edges {
--     --                             node {
--     --                                 topic {
--     --                                 name
--     --                                 }
--     --                             }
--     --                         }
--     --                     }
--     --                     }
--     --                 }
--     --                 }
--     --             }
--     --         }
--     --     |]
--     -- )
--   print "q"
-- --   qa <- runQ q
--   qa <- fetch $ resolver "fake"
-- --   print @Text $ _ q
--   print "Apple"

-- fetchHero :: Args GetRepo -> IO (Either String GetRepo)
fetchHero :: IO (Either String GetRepo)
fetchHero = fetch jsonRes $ args
 where
  args :: GetRepoArgs
  args = GetRepoArgs { org = "Zeus" }
  jsonRes :: ByteString -> IO ByteString
  jsonRes = return

fetchRepo :: IO (Either String GetRepo)
fetchRepo = fetch (resolver "fake") args
  where args = GetRepoArgs { org = "Google" }
-- defineByDocumentFile'
--     (makeRelativeToProject "src/GraphQL/Mythology.gql")
--     [gql|
--         query GetHero ($character: Character)
--         {
--             deity (fatherOf:$character) {
--             name
--             power
--             worships {
--                 deity2Name: name
--             }
--             }
--         }
--     |]

runRepo :: IO ()
runRepo = do
  (Right repo) <- fetchRepo
  print repo
