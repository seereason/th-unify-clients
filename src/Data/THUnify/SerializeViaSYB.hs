{-# LANGUAGE CPP, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, RankNTypes, ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeOperators #-}

module Data.THUnify.SerializeBySYB
    ( putData
    , getData
    ) where

import Control.Monad (msum, when)
import Data.ByteString as S
import Data.ByteString.Lazy as L
import Data.Bytes.Get (getWord8)
import Data.Bytes.Put (putWord8)
import Data.Data
import Data.Serialize (Get, Put, get, put, Serialize, encode, decode)
import Data.Serialize.Get (runGet, runGetLazy)
import Data.Serialize.Put (runPut, runPutLazy)
import Data.Typeable
import Data.Word (Word8)
import GHC.Generics (Generic)
import Language.Haskell.TH (ExpQ, TypeQ, Q, Dec)
import Test.QuickCheck
import Unsafe.Coerce (unsafeCoerce)

-- We need a wrapper class so that we have a method
-- to call on the subterms.
class GSerialize a where
  gput :: a -> Put
  gget :: Get a

-- | These two functions implement the Serialize put and get methods for
-- any algebraic type.
putData :: forall a. (Data a, Typeable a) => a -> Put
putData a =
    let t :: DataType
        t = dataTypeOf a in
    case () of
      _ | isAlgType t -> do
              when (maxConstrIndex t > 1) (putWord8 (fromIntegral (pred (constrIndex (toConstr a)))))
              _ <- gmapM (\d -> putData d >> return d) a
              return ()
      otherwise ->
          maybe (error ("putData " ++ show (typeOf a))) id $
          msum [fmap put (cast a :: Maybe Int),
                fmap put (cast a :: Maybe Char),
                fmap put (cast a :: Maybe Float),
                fmap put (cast a :: Maybe Double)]

getData :: forall a. (Data a, Typeable a) => Get a
getData =
    let t = dataTypeOf (undefined :: a) in
    case () of
      _ | isAlgType t -> do
              i <- if maxConstrIndex t > 1 then getWord8 else return 0
              fromConstrM getData (indexConstr t (succ (fromIntegral i)))
      otherwise ->
          maybe (error ("Data.Path.getData " ++ show (typeRep (Proxy :: Proxy a)))) id $
          -- msum $(listE (fmap getPrimitive primitiveTypes))
          msum [maybe Nothing (\Refl -> Just (fmap (unsafeCoerce :: Int -> a) get)) (eqT :: Maybe (a :~: Int)),
                maybe Nothing (\Refl -> Just (fmap (unsafeCoerce :: Char -> a) get)) (eqT :: Maybe (a :~: Char)),
                maybe Nothing (\Refl -> Just (fmap (unsafeCoerce :: Float -> a) get)) (eqT :: Maybe (a :~: Float)),
                maybe Nothing (\Refl -> Just (fmap (unsafeCoerce :: Double -> a) get)) (eqT :: Maybe (a :~: Double))]

primitiveTypes :: [TypeQ]
primitiveTypes = [[t|Int|], [t|Char|], [t|Float|], [t|Double|]]

getPrimitive :: TypeQ -> TypeQ -> ExpQ
getPrimitive a typ = [|maybe Nothing (\Refl -> Just (fmap (unsafeCoerce :: $typ -> $a) get)) (eqT :: Maybe ($a :~: $typ))|]

putPrimitive :: TypeQ -> ExpQ
putPrimitive typ = [|fmap put (cast a :: Maybe $typ)|]

deriveSerializeAlgebraic :: TypeQ -> Q [Dec]
deriveSerializeAlgebraic typ =
    [d|instance Serialize $typ where
          put = putData
          get = getData|]

data Foo = Foo Int Char | Bar Float Double deriving (Data, Typeable, Show, Generic, Serialize, Eq)

instance Arbitrary Foo where
  arbitrary = oneof [Foo <$> arbitrary <*> arbitrary, Bar <$> arbitrary <*> arbitrary]

encode :: Data a => a -> S.ByteString
encode = runPut . putData

-- | Encode a value using binary serialization to a lazy ByteString.
encodeLazy :: Data a => a -> L.ByteString
encodeLazy  = runPutLazy . putData

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.
decode :: Data a => S.ByteString -> Either String a
decode = runGet getData

-- | Decode a value from a lazy ByteString, reconstructing the original
-- structure.
decodeLazy :: Data a => L.ByteString -> Either String a
decodeLazy  = runGetLazy getData

prop_test_new_encode :: (Serialize a, Data a, Eq a, a ~ Foo) => a -> Bool
prop_test_new_encode x = Right x == Data.Serialize.decode (Data.THUnify.SerializeBySYB.encode x)

prop_test_new_decode :: (Serialize a, Data a, Eq a, a ~ Foo) => a -> Bool
prop_test_new_decode x = Right x == Data.THUnify.SerializeBySYB.decode (Data.Serialize.encode x)

-- main = putStrLn . show . putData (Foo 3 'x')
-- quickCheck prop_test_new_encode >> quickCheck prop_test_new_decode
