{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans -ddump-minimal-imports #-}

module Data.THUnify.Orphans where

import Data.ByteString (ByteString)
import Data.ListLike as LL
import Data.Proxy (Proxy(Proxy))
import Data.Text (pack, unpack)
import Data.THUnify.Prelude.Orphans ()
import Language.Haskell.TH
import Language.Haskell.TH.PprLib (ptext)
import Prelude hiding (concat, foldl1)
import Test.QuickCheck (Arbitrary(arbitrary))
import Text.Parsec (count, many)
--
import Web.Routes
--import Language.Haskell.TH.TypeGraph.WebRoutesTH (derivePathInfo)

-- The type parameter in Proxy t does not itself need to be an
-- instance of PathInfo, because there is no actual t value in the
-- Proxy value.  The derivePathInfo function does not realize this.
-- $(derivePathInfo ''Proxy)
instance {-PathInfo t =>-} PathInfo (Proxy t) where
    toPathSegments inp = case inp of Proxy -> [pack "proxy"]
    fromPathSegments = segment (pack "proxy") >> return Proxy

instance PathInfo ByteString where
    toPathSegments bs = [pack $ show bs]
    fromPathSegments = (read . unpack) <$> anySegment

#if 0
$(derivePathInfo ''Loc)
#else
instance PathInfo Loc where
    toPathSegments _ = []
    fromPathSegments = pure noLoc
        where
          noLoc = Loc "" "" "" (0, 0) (0, 0)
#endif
instance PathInfo (Int, Int) where
    toPathSegments (a, b) = toPathSegments [a, b]
    fromPathSegments = (\[a, b] -> (a, b)) <$> count 2 fromPathSegments
instance PathInfo [Loc] where
    toPathSegments = concat . fmap toPathSegments
    fromPathSegments = many fromPathSegments
instance PathInfo [Int] where
    toPathSegments = concat . fmap toPathSegments
    fromPathSegments = many fromPathSegments

#if 0
$(deriveMemoizable ''Type)
$(deriveMemoizable ''TyVarBndr)
$(deriveMemoizable ''Name)
$(deriveMemoizable ''NameFlavour)
$(deriveMemoizable ''TyLit)
$(deriveMemoizable ''OccName)
$(deriveMemoizable ''NameSpace)
$(deriveMemoizable ''PkgName)
$(deriveMemoizable ''ModName)
#endif

instance Ppr ByteString where ppr = ptext . show
instance Arbitrary ByteString where arbitrary = pure mempty
