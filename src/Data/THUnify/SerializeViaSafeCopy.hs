{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.THUnify.SerializeViaSafeCopy where

import Data.SafeCopy
import Data.Serialize

instance SafeCopy a => Serialize a where
  get = safeGet
  put = safePut
