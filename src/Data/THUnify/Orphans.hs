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
import Data.Generics (TyCon, TypeRep, tyConFingerprint, tyConModule, tyConName, tyConPackage, splitTyConApp)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.ListLike as LL
import Data.Map as Map (Map, toList)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set (Set, toList)
import Data.Text (pack, unpack)
import Debug.Show (V(V))
import Language.Haskell.TH
import Language.Haskell.TH.PprLib (Doc, hcat, ptext, vcat)
import Language.Haskell.TH.Syntax (ModName, NameFlavour, OccName, PkgName)
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

instance Ppr (Type, Int32) where
  ppr (t, n) = pprPair (t, n)

instance Ppr Int32 where ppr = ptext . show

instance Ppr (Name, [Type]) where
    ppr (name, params) = ppr (foldl1 AppT (ConT name : params))

pprPair :: (Ppr a, Ppr b) => (a, b) -> Doc
pprPair (a, b) = hcat [ptext "(", ppr a, ptext ",", ppr b, ptext ")"]

pprList :: [Doc] -> Doc
pprList xs = hcat [ptext "[", hcat (intersperse (ptext ",") xs), ptext "]"]

-- deriving instance Data CmdSpec

#if 0
instance Arbitrary ReportImageID where arbitrary = ReportImageID <$> arbitrary
instance Arbitrary ReportElemID where arbitrary = ReportElemID <$> arbitrary
instance (Arbitrary v, Enum k, Ord k) => Arbitrary (Order k v) where
    arbitrary = sized $ \n -> do
      vs <- vectorOf n (arbitrary :: Gen v)
      ks <- shuffle (take n [toEnum 0..] :: [k])
      fromPairs <$> shuffle (zip ks vs)
#endif

instance Ppr Char where ppr = ptext . show
instance Ppr Float where ppr = ptext . show
-- instance Ppr ReportElemID where ppr = ptext . show
-- instance Ppr ReportImageID where ppr = ptext . show
instance (Ppr k, Ord k, {-Enum k,-} Show k, Ppr v) => Ppr (Map k v) where ppr = pprList . fmap pprPair . Map.toList
#if 0
instance (Ppr k, Ord k, Enum k, Show k, Ppr v) => Ppr (Order k v) where ppr = pprList . fmap pprPair . LL.toList . toPairs
#endif
instance Ppr (Int, Char) where ppr = ptext . show
instance Ppr ByteString where ppr = ptext . show
instance Arbitrary ByteString where arbitrary = pure mempty

instance Ppr Bool where
    ppr True = ptext "True"
    ppr False = ptext "False"

instance Show (V TyCon) where
    show (V c) =
        "TyCon {" ++
        "module=" ++ show (tyConModule c) ++
        " name=" ++ show (tyConName c) ++
        " package=" ++ show (tyConPackage c) ++
        " fingerprint=" ++ show (tyConFingerprint c) ++ "}"

instance Show (V TypeRep) where
    show (V r) =
        case splitTyConApp r of
          (c, rs) -> "[" ++ intercalate "," (show (V c) : fmap (show . V) rs) ++ "]"

instance Ppr TypeRep where
  ppr = ptext . show

instance Ppr () where
    ppr () = ptext "()"

-- | 'Int' is the 'Data.Path.Index.ContainerKey' type for all lists, so
-- we need to make sure all the required instances exist.
instance Ppr Int where
    ppr = ptext . show

instance Ppr (Set Type, Set Type) where
    ppr (extra, missing) = vcat [ptext "extra:", ppr extra, ptext "missing:", ppr missing]

instance Ppr (Set Type) where
    ppr s = hcat [ptext "Set.fromList [", ppr (Set.toList s), ptext "]"]

$(deriveSafeCopy 0 'base ''OccName)
$(deriveSafeCopy 0 'base ''NameSpace)
$(deriveSafeCopy 0 'base ''PkgName)
$(deriveSafeCopy 0 'base ''ModName)
$(deriveSafeCopy 0 'base ''NameFlavour)
$(deriveSafeCopy 0 'base ''Name)
$(deriveSafeCopy 1 'base ''Loc)
