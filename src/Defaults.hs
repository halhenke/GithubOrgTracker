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
-- {-# LANGUAGE OverloadedLists #-}
-- {-@ LIQUID "--exact-data-cons" @-}
-- {-@ LIQUID "--stringtheory" @-}
-- {-@ LIQUID "--prune-unsorted" @-}
{-|
    This module has the Default values for general things
 -}

module Defaults where

-- import           Prelude                       as R
import           Relude                        as R
                                         hiding ( Int )
import           Data.Foldable                 as DF
-- import           Prelude
-- import           Data.Foldable
-- import           Data.Text
import           GHC.Types
-- import           GHC.Types.Int
-- import           Data.Text                     as DT
-- import           Data.Vector                   as DV
-- import           GHC.TypeLits
-- import           Lick.Liquid                   as LL

-- | Define the size
-- {-@ measure vlen :: Vector a -> Int @-}
{-@ measure vlen :: Data.Foldable.Foldable t => t a -> Int @-}

{-@ measure len :: forall a. [a] -> GHC.Types.Int @-}
len ([]    ) = 0
len (y : ys) = 1 + (len ys)

 {-@ predicate NonNull X = ((len X) > 0) @-}

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
{-@ assume length :: Data.Foldable.Foldable f => forall a. xs:f a -> {v:Int | v = vlen xs} @-}

{-@ type NonZeroList a = Data.Foldable.Foldable t => { v:t a | vlen v > 0 }  @-}

{-@ type NZL a = { v:[a] | vlen v > 0 }  @-}

-- {-@ embed NZL * as int @-}

-- {-@ ltonzl :: Data.Foldable.Foldable t => t a -> NonZeroList a @-}
-- ltonzl a = a

-- {-@ invariant {v:Vector a | 0 <= size v} @-}
-- {-@ defaultOrgs :: {d:[Text] | len d > 0}  @-}
-- {-@ defaultOrgs :: NonZeroList Text @-}
-- {-@ defaultOrgs :: {d:NonZeroList [GHC.Types.Char] | vlen d > 0}  @-}
-- {-@ defaultOrgs :: NonZeroList String @-}
-- {-@ defaultOrgs :: [Text] @-}
-- {-@ defaultOrgs ::{ v:[Text] | NonNull v } @-}
-- defaultOrgs :: NEVector Text
-- defaultOrgs = (DV.fromList :: [a] -> DV.Vector a)
-- {-@ defaultOrgs :: NonZeroList Text @-}
-- {-@ defaultOrgs :: NZL Text @-}
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


-- {-@ dooList :: NonZeroList String @-}
-- {-@ dooList :: NZL String @-}
-- {-@ dooList :: { v:[String] | NonNull(v) }  @-}
-- {-@ dooList :: [String] @-}
dooList :: [String]
-- dooList :: DF.Foldable t => t String
dooList = ["Abba", "Babba"]
