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
-- {-@ LIQUID "--exact-data-cons" @-}
{-|
    This module has the Default values for general things
 -}

module Lick.Liquid
  ()
where

import           Prelude                       as R
                                         hiding ( Text )
-- import           Relude                        as R
-- import           GHC.Num
import           Data.Vector                   as DV
                                                ( length
                                                , (!)
                                                , fromList
                                                )
import           Data.Text                     as DT
import           GHC.TypeLits

-- -- | Define the size
-- {-@ measure vlen :: Vector a -> Int @-}

-- -- | Compute the size
-- {-@ assume length :: x:Vector a -> {v:Int | v = vlen x} @-}

-- | Lookup at an index
{-@ assume (!) :: x:Vector a -> {v:Nat | v < vlen x} -> a @-}

{-@ type NEVector a = {v:Vector a | 0 < vlen v} @-}

{-@ type VectorN a N = {v:Vector a | vlen v == N} @-}

{-@ twoLangs :: VectorN String 2 @-}
twoLangs = fromList ["haskell", "javascript"]

{-@ type Btwn Lo Hi = {v:Int | Lo <= v && v < Hi} @-}
