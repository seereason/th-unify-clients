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
import Data.Vector (Vector)
import Data.UserId (UserId)
import Data.Time (UTCTime)
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

#if 1
-- The old Path types

data TypePath t s a = TypePath [Hop SerializedIndex] deriving (Eq, Ord, Data, Typeable)

-- | 'TypePath' with no phantom end type.
data TypeSPath t s = TypeSPath [Hop SerializedIndex] deriving (Eq, Ord, Data, Typeable)

-- | 'TypePath' with no phantom types.
data TypeUPath t = TypeUPath [Hop SerializedIndex] deriving (Show, Eq, Ord, Data, Typeable)

-- | 'TypePath' with no phantom types.
data TypeEPath t a = TypeEPath [Hop SerializedIndex] deriving (Show, Eq, Ord, Data, Typeable)
#else
-- The new Path types

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
#endif

-- A type which contains a type with phantom type variables.
data PathValue t3 s3 =
    PathValue {
      sPath :: TypeSPath t3 s3,
      encodedValue :: ByteString
    } deriving (Eq, Ord, Data, Typeable, Show)

deriving instance Show (TypeSPath paths s)

data PathError
    = CastFailure -- ^ Cast failed in a place we thought it couldn't
    | DecodeFailure String ByteString -- ^ Failed to decode a value
    | BadValueInstance -- ^ A Value instance method is wrong
    | BadPathsInstance -- ^ The Paths instance method is wrong
    deriving (Show, Data, Eq, Ord)

data EditError k v a t s
    = OrderKeysMismatch {_eePath :: TypeSPath t s, _expectedKeys :: Vector k, _actualKeys :: Vector k} -- Someone else reordered during a reorder
    | NotAPermutation {_eePath :: TypeSPath t s, _originalKeys :: Vector k, _permutedKeys :: Vector k} -- Something got deleted or inserted during a reorder
    | UpdateValueMismatch {_eePath :: TypeSPath t s, _expected :: a, _actual :: a} -- typical simultaneous edit conflict
    | AlreadyDeleted {_eePath :: TypeSPath t s, _originalKeys :: Vector k, _deleteKey :: k}-- Someone already deleted that (do we care?)
    | AlreadyInserted {_eePath :: TypeSPath t s, _originalKeys :: Vector k, _insertKey :: k} -- Someone already inserted something there (but maybe its the same thing?)
    | CantInsertThere {_eePath :: TypeSPath t s, _originalKeys :: Vector k, _insertPos :: Int} -- The insert position no longer exists (maybe just append?)
    | CantDeleteThere {_eePath :: TypeSPath t s, _originalKeys :: Vector k, _insertPos :: Int} -- The delete position no longer exists (does this matter?)
    | CantUpdateThere {_eePath :: TypeSPath t s} -- The position you want to update is gone
    | DeleteValueMismatch {_eePath :: TypeSPath t s, _expectedValue :: v, _actualValue :: v} -- Someone edited the thing you're deleting
    -- Remaining constructors represent type errors - should be impossible
    | UpdateTypeMismatch {_eePath :: TypeSPath t s}
    | NotAnOrder {_eePath :: TypeSPath t s}
    | OrderKeyTypeMismatch {_eePath :: TypeSPath t s}
    | OrderValueTypeMismatch {_eePath :: TypeSPath t s}
    -- | EditErrorList [EditError k v a t s]
    | PathError PathError -- Need a migration for this?
    | ValueError {_eeStart :: String, _eeValueString :: String, _eePathHops :: [String]}
    | ErrorString String
    | UnexpectedEmptyTraversal

type EditErrorMono t s = EditError SerializedIndex SerializedIxValue SerializedValue t s

data Op k v a t s
    = Reorder {_opNewKeys :: Vector k, _opPath :: TypeSPath t s}
    -- ^ Change the order of the list without changing the key/value
    -- associations
    | Delete {_opKey :: k, _opPath :: TypeSPath t s}
    -- ^ Delete the element associated with a key
    | Update {_opPath :: TypeSPath t s, _opValue :: a}
    -- ^ Replace old with new
    | Append {_opPath :: TypeSPath t s, _opIxValue :: v}
    -- ^ Insert a value at the next unoccupied position
    | Insert {_opPos :: Int, _opPath :: TypeSPath t s, _opIxValue :: v}
    -- ^ Insert a value at position n, a new key is allocated
    -- using an Enum instance.
    | InsertWithKey {_opKey :: k, _opPos :: Int, _opPath :: TypeSPath t s, _opIxValue :: v}
    -- ^ Insert a (key, value) pair at position n
    deriving Typeable
data Edit k v a t s
    = Updated {_oldV :: a, _newV :: a, _ePath :: TypeSPath t s}
    -- ^ Replace old with new
    | Reordered {_oldO :: Vector k, _newO :: Vector k, _ePath :: TypeSPath t s}
    -- ^ Update the order of the keys.
    | Inserted {_oldO :: Vector k, _newO :: Vector k, _eKey :: k, _insV :: v, _ePos :: Int, _ePath :: TypeSPath t s}
    -- ^ Insert an element at position n
    | Deleted {_oldO :: Vector k, _newO :: Vector k, _eKey :: k, _delV :: v, _ePos :: Int, _ePath :: TypeSPath t s}
    -- ^ Delete the element at position n

newtype EventId = EventId {unEventId :: Int}
    deriving (Eq, Ord, Read, Show, Data)

data Event k v a t s
    = Event
      { _user :: UserId
      -- ^ The event's creator
      , _when :: UTCTime
      -- ^ Time stamp assigned by the client as soon as possible after
      -- the user input that created the event.
      , _eventId :: EventId
      -- ^ A sequence number assigned by the server when the event
      -- becomes part of a History.
      , _edit :: Edit k v a t s
      }

instance Functor (Event k v a t) where
    fmap f (Event u t a e) = Event u t a (fmap f e)

instance Functor (Edit k v a t) where
    fmap _ (Updated old new (TypeSPath hs)) = Updated old new (TypeSPath hs)
    fmap _ (Reordered old new (TypeSPath hs)) = Reordered old new (TypeSPath hs)
    fmap _ (Inserted old new k v i (TypeSPath hs)) = Inserted old new k v i (TypeSPath hs)
    fmap _ (Deleted old new k v i (TypeSPath hs)) = Deleted old new k v i (TypeSPath hs)

newtype SerializedIxValue = SerializedIxValue {unSerializedIxValue :: ByteString} deriving (Data, Eq, Ord, Show)
newtype SerializedValue = SerializedValue {unSerializedValue :: ByteString} deriving (Data, Eq, Ord, Show)

type EventMono = Event SerializedIndex SerializedIxValue SerializedValue

data EventTree t s
    = Current
      { _events :: [EventMono t s]
      -- ^ Most recent event first
      , _value :: s
      -- ^ Result of applying all the events to some initial value.
      }
    | Previous
      { _events :: [EventMono t s]
      -- ^ EventTree that occurred before some conflict caused the
      -- split that creating the branches.
      , _branches :: [EventTree t s]
      } deriving (Functor)

data HistoryTree_1 t s
    = HistoryTree {_eventTree :: EventTree t s, _nextAccession :: EventId}
    deriving (Functor)
