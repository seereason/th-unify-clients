-- | Create a Serialize instance based on GHC.Generics (and create the
-- Generic instance as well.)

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.THUnify.GHCGenerics
    ( deriveSerialize
    ) where

import Data.Serialize (Serialize)
import GHC.Generics
import Language.Haskell.TH (Dec, Q, TypeQ)

deriveSerialize :: TypeQ -> Q [Dec]
deriveSerialize typ =
    [d|deriving instance Generic $typ
       deriving instance Serialize $typ|]
