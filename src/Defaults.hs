{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE PackageImports              #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}
{-|
    This module has the Default values for general things
 -}

module Defaults where

-- import           Prelude                       as R
import           Relude                        as R
                                         hiding ( Int )
import           Data.Foldable                 as DF


defaultOrgs :: [Text]
defaultOrgs =
  [ "google"
  , "google-research"
  , "PAIR-code"
  , "facebook"
  , "microsoft"
  , "deepmind"
  , "rapidsai"
  , "openai"
  , "JuliaMath"
  , "JuliaData"
  , "queryverse"
  , "kowainik"
  , "tweag"
  , "aws"
  , "awslabs"
  , "FluxML"
  , "uber-research"
  ]

defaultOrg :: String
defaultOrg = "Google"

githubOrgDb :: FilePath
githubOrgDb = "haskell-git-org.sqlite"

allTableNams :: [Text]
allTableNams = ["org", "repo", "repoQuery"]