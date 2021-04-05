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


module GraphQL.HelperTH
  ( defineByDocumentFile'
  )
where


import           Language.Haskell.TH
import           Data.Morpheus.Types.Internal.AST
                                               as DMT
import           Data.Morpheus.Client          as DMC

import           Relude


-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile'
  :: Q FilePath -> (DMT.ExecutableDocument, String) -> Q [Dec]
defineByDocumentFile' qFilePath args =
  qFilePath >>= flip defineByDocumentFile args
