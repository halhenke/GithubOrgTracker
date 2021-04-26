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

{-|
    This module has the Default values for general things
 -}

module Defaults where

import           Relude                        as R

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
  , "FluxML"
  ]

defaultOrg :: Text
defaultOrg = "Google"

github_org_db :: FilePath
github_org_db = "haskell-git-org.sqlite"

allTableNams :: [Text]
allTableNams = ["org", "repo", "repoQuery"]
