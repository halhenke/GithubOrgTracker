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


module GraphQL.API
  -- ( runRepo
  -- )
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

import           Lens.Micro                    as LM
import           Lens.Micro.TH                 as LMTH
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

newtype HTML = HTML {getHTML :: Text}
    deriving (Typeable, Eq, Show)
    -- deriving (Typeable, DecodeScalar, EncodeScalar, Eq, Show)
newtype DateTime = DateTime {getDateTime :: Text}
    deriving (Typeable, Eq, Show)
    -- deriving (Typeable, DecodeScalar, EncodeScalar, Eq, Show)

instance EncodeScalar HTML where
  encodeScalar (HTML a) = String a

instance DecodeScalar HTML where
  decodeScalar (String t) = Right (HTML t)
  decodeScalar _          = Left "HTML parse failed"
-- githubApi :: ByteString -> IO ByteString
-- githubApi req = do
--   print req
--   return "bad"

instance EncodeScalar DateTime where
  encodeScalar (DateTime t) = String t

instance DecodeScalar DateTime where
  decodeScalar (String t) = Right (DateTime t)
  decodeScalar _          = Left "DateTime parse failed"


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

makeLensesFor [("repositories", "_repositories")] ''OrganizationOrganization
makeLensesFor [("edges", "_edges")] ''OrganizationRepositoriesRepositoryConnection
makeLensesFor [("node", "_node")] ''OrganizationRepositoriesEdgesRepositoryEdge
makeLensesFor [("name", "_name"), ("description", "_description"), ("stargazers", "_stargazers"), ("primaryLanguage", "_primaryLanguage"), ("createdAt", "_createdAt"), ("pushedAt", "_pushedAt"), ("updatedAt", "_updatedAt"), ("languages", "_languages"), ("repositoryTopics", "_repositoryTopics")] ''OrganizationRepositoriesEdgesNodeRepository
makeLensesFor [("totalCount", "_totalCount")] ''OrganizationRepositoriesEdgesNodeStargazersStargazerConnection

fetchHero :: IO (Either String GetRepo)
fetchHero = fetch jsonRes $ args
 where
  args :: GetRepoArgs
  args = GetRepoArgs { org = "google" }
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

-- parse :: OrganizationOrganization -> [OrganizationRepositoriesEdgesRepositoryEdge]
-- parse :: OrganizationOrganization -> [OrganizationRepositoriesEdgesNodeRepository]
parse :: OrganizationOrganization -> [Text]
parse oo = cc -- & b
 where
  -- cc :: _ccc
  cc =
    oo
      ^. _repositories
      ^. _edges
      &  (^?! _Just)
      &  (^.. each
           . _Just
           . _node
           . _Just
           . (_name <> (_updatedAt . (to getDateTime)))
         )
  -- a  = _aaa b
  -- b = (^. _node) d
  -- b  = (^.. each . _node) & d
  -- d  = (^.. each . _Just) -- & e
  -- d = (^?! _Just) (^. _name)
  -- e  = (^.. each . _name)

  -- b  = (^.. _name)
-- ^. _1 ^
-- parseOrg :: OrganizationOrganization ->
