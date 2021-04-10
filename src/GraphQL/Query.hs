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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}


module GraphQL.Query where


import           Relude                         ( Eq
                                                , Show
                                                , Typeable
                                                , Int
                                                , Either(Left, Right)
                                                , String
                                                , Text
                                                )
import           Data.Morpheus.Client          as DMC
                                                ( gql
                                                , DecodeScalar(..)
                                                , EncodeScalar(..)
                                                , ScalarValue(String)
                                                )
import           GraphQL.HelperTH               ( defineByDocumentFile' )
-- import           Language.Haskell.TH
import           Data.FileEmbed                 ( makeRelativeToProject )
import           Control.Lens                  as LM
                                                ( makeLensesFor )


newtype HTML = HTML {getHTML :: Text}
    deriving (Typeable, Eq, Show)
newtype DateTime = DateTime {getDateTime :: Text}
    deriving (Typeable, Eq, Show)

instance EncodeScalar HTML where
  encodeScalar (HTML a) = String a

instance DecodeScalar HTML where
  decodeScalar (String t) = Right (HTML t)
  decodeScalar _          = Left "HTML parse failed"

instance EncodeScalar DateTime where
  encodeScalar (DateTime t) = String t

instance DecodeScalar DateTime where
  decodeScalar (String t) = Right (DateTime t)
  decodeScalar _          = Left "DateTime parse failed"

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
