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
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}
-- {-@ LIQUID "--exact-data-cons" @-}
{-|
    This module has the Default values for general things
 -}

module Defaults where

-- import           Prelude                       as R
import           Relude                        as R
                                                ( Text )
import           Prelude
import           Data.Foldable
-- import           Data.Text
-- import           GHC.Num
-- import           Data.Text                     as DT
-- import           Data.Vector                   as DV
-- import           GHC.TypeLits
-- import           Lick.Liquid                   as LL

-- | Define the size
-- {-@ measure vlen :: Vector a -> Int @-}
{-@ measure vlen :: Data.Foldable.Foldable t => t a -> Int @-}

-- | Compute the size
-- {-@ assume length :: x:Vector a -> {v:Int | v = vlen x} @-}

-- | Lookup at an index
-- {-@ assume (!) :: x:Vector a -> {v:Nat | v < vlen x} -> a @-}

-- {-@ type NEVector a = {v:Vector a | 0 < vlen v} @-}

-- deriving instance Ord Int
-- deriving instance Num Int

-- {-@ measure vlen :: [a] -> Int  @-}
-- vlen []       = 0
-- vlen (a : as) = 1 + vlen as
-- {-@ assume length ::  forall (t :: * -> *) a. Data.Foldable.Foldable t => x:t a -> {v:Int | v = vlen x} @-}
{-@ assume length :: Data.Foldable.Foldable f => forall a. xs:f a -> {v:Nat | v = vlen xs} @-}

{-@ type NonZeroList a = Data.Foldable.Foldable t => { v:t a | vlen v > 0 }  @-}

{-@ type NZL a = { v:[a] | vlen v > 0 }  @-}

-- {-@ ltonzl :: Data.Foldable.Foldable t => t a -> NonZeroList a @-}
-- ltonzl a = a

-- {-@ invariant {v:Vector a | 0 <= size v} @-}
-- {-@ defaultOrgs :: {d:[Text] | vlen d > 0}  @-}
-- {-@ defaultOrgs :: NonZeroList Text @-}
-- {-@ defaultOrgs :: {d:NonZeroList [GHC.Types.Char] | vlen d > 0}  @-}
-- {-@ defaultOrgs :: NonZeroList String @-}
-- {-@ defaultOrgs :: [Text] @-}
-- defaultOrgs :: NEVector Text
-- defaultOrgs = (DV.fromList :: [a] -> DV.Vector a)
-- {-@ defaultOrgs :: NonZeroList String @-}
-- {-@ defaultOrgs :: [String] @-}
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
  , "FluxML"
  ]

defaultOrg :: String
defaultOrg = "Google"

github_org_db :: FilePath
github_org_db = "haskell-git-org.sqlite"

allTableNams :: [Text]
allTableNams = ["org", "repo", "repoQuery"]


{-@ dooList :: [String] @-}
dooList = ["Abba", "Babba"]
