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


module GraphQL.API
  ()
where

import           Data.Morpheus.Client          as DMC
import           Data.FileEmbed
-- import           Relude
import           Relude
import           GraphQL.HelperTH
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



defineByDocumentFile'
    (makeRelativeToProject "src/GraphQL/Mythology.gql")
    [gql|
        query GetHero ($character: Character)
        {
            deity (fatherOf:$character) {
            name
            power
            worships {
                deity2Name: name
            }
            }
        }
    |]
