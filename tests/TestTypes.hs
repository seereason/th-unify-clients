{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}

module TestTypes where

import Control.Lens ((%=))
import qualified Control.Monad.RWS as MTL (get, local)
import Data.ByteString (ByteString)
import Data.Generics
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.SafeCopy
import Data.THUnify.Monad (applied, M, pprint1, R(R), runV, tyvars)
import Data.THUnify.Traverse (SubstFn, withBindings, withTypeExpansions)
import Data.Serialize (get, put, Serialize)
import Extra.Orphans
import Language.Haskell.TH (Exp(..), ExpQ, runQ, TypeQ, TyVarBndr)
import Language.Haskell.TH.Lift (lift)

newtype SerializedIndex = SerializedIndex {unSerializedIndex :: ByteString} deriving (Data, Eq, Ord, Show)

data Order_0 k v =
    Order_0 { elems_0 :: Map k v
          -- ^ Return the key-value map
          , order_0 :: [k]
          -- ^ Return the list of keys in order.
          , next_0 :: k
          -- ^ Next available key
          }
    deriving (Data, Typeable, Functor)

type L a = Seq a

data VMap k v =
  VMap
    { _map :: Map k v
    , _vec :: L k
    } deriving (Data, Typeable, Functor, Read, Show)

-- A type with a non-phantom type variable
data Hop key
    = FieldHop Int Int
      -- ^ Hop to one of the fields of a record - the constructor and
      -- field number are specified.  Also handles tuples, which are
      -- single constructor types.
    | IxHop key
    -- ^ Hop from an instance of 'Ixed', such as @[]@ or @Map@, via
    -- some key of the corresponding 'Index' type, to a value of the
    -- corresponding 'IxValue' type.  For serialization the
    -- 'Data.Serialize.encode' function is applied to the key to make
    -- this a monomorphic type @Hop SerializedIndex@.
    | AtHop key
    -- ^ Hop from an instance of At, which is a subclass of Ixed
    | ViewHop
    deriving (Show, Eq, Ord, Functor, Data, Typeable)

-- A type with phantom type variables
data Path t2 s2 a2 =
    Path
    { _start :: s2
    , _hoplist :: [Hop SerializedIndex]
    , _end :: a2
    } deriving (Eq, Ord, Data, Typeable)

-- | Some type alises of Path.
type TypePath t1 s1 a1 = Path t1 (Proxy s1) (Proxy a1)
type TypeSPath t1 s1 = Path t1 (Proxy s1) ()
type TypeUPath t = Path t () ()
type TypeEPath t a = Path t () (Proxy a)

-- A type which contains a type with phantom type variables.
data PathValue t3 s3 =
    PathValue {
      sPath :: TypeSPath t3 s3,
      encodedValue :: ByteString
    } deriving (Eq, Ord, Data, Typeable, Show)

deriving instance Show (TypeSPath paths s)
