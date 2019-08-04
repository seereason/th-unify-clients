{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor, DeriveGeneric, FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}

module TestTypes where

import Control.Lens ((%=))
import qualified Control.Monad.RWS as MTL (get, local)
import Data.ByteString (ByteString)
import Data.Generics (Data, Typeable)
import Data.Int
import Data.Map (Map)
import Data.Order (Order)
import Data.SafeCopy
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.THUnify.Traverse (applied, M, pprint1, runV, tyvars)
import Data.THUnify.Traverse (SubstFn, withBindings, withTypeExpansions)
import Data.Serialize (get, put, Serialize)
import Data.Vector (Vector)
import Data.UserId (UserId)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Extra.Orphans
import Extra.Time (Zulu(..))
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp(..), ExpQ, runQ, TypeQ, TyVarBndr)
import Language.Haskell.TH.Lift (lift)
import Network.URI (URI(..), URIAuth(..))
import Text.LaTeX (LaTeX)
import Text.LaTeX.Base.Syntax as LaTeX (LaTeX(..), Measure(..), TeXArg(..), MathType(..))
import qualified Text.Pandoc.Definition as P

deriving instance Read URI
deriving instance Read URIAuth
deriving instance Ord Measure
deriving instance Read Measure
deriving instance Ord LaTeX
deriving instance Ord TeXArg
deriving instance Ord MathType
deriving instance Read MathType
deriving instance Read TeXArg
deriving instance Read LaTeX
deriving instance Read Zulu

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
    deriving (Generic)

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
    deriving (Generic, Typeable)
data Edit k v a t s
    = Updated {_oldV :: a, _newV :: a, _ePath :: TypeSPath t s}
    -- ^ Replace old with new
    | Reordered {_oldO :: Vector k, _newO :: Vector k, _ePath :: TypeSPath t s}
    -- ^ Update the order of the keys.
    | Inserted {_oldO :: Vector k, _newO :: Vector k, _eKey :: k, _insV :: v, _ePos :: Int, _ePath :: TypeSPath t s}
    -- ^ Insert an element at position n
    | Deleted {_oldO :: Vector k, _newO :: Vector k, _eKey :: k, _delV :: v, _ePos :: Int, _ePath :: TypeSPath t s}
    -- ^ Delete the element at position n
    deriving (Generic, Data)

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
      } deriving (Generic, Data)

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
      { _events1 :: [EventMono t s]
      -- ^ Most recent event first
      , _value1 :: s
      -- ^ Result of applying all the events to some initial value.
      }
    | Previous
      { _events1 :: [EventMono t s]
      -- ^ EventTree that occurred before some conflict caused the
      -- split that creating the branches.
      , _branches :: [EventTree t s]
      } deriving (Generic, Data, Functor)

data HistoryTree_1 t s
    = HistoryTree {_eventTree :: EventTree t s, _nextAccession :: EventId}
    deriving (Generic, Data, Functor)

data History t s
    = History { _value :: s
              , _events :: [EventMono t s]
              , _nextId :: EventId
              } deriving (Generic, Data, Functor)

newtype ReportID = ReportID { unReportID :: UUID } deriving (Generic, Eq, Ord, Typeable, Data, Read, Show)
type IntJS = Int32

data Branding
    = NoLogo
    | Logo ImageFile
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

#if 0
data Report = Report {reportData :: Int} deriving (Generic, Eq, Ord, Typeable, Data)
#else
data Report
    = Report { _reportFolder :: FilePath
             , _reportName :: Markup
             , _reportDate :: Markup
             , _reportContractDate :: Markup
             , _reportInspectionDate :: Markup
             , _reportEffectiveDate :: Markup
             , _reportAuthors :: Authors
             , _reportPreparer :: Markup
             , _reportPreparerEIN :: Markup
             , _reportPreparerAddress :: Markup
             , _reportPreparerEMail :: Markup
             , _reportPreparerWebsite :: Markup
             , _reportAbbrevs :: AbbrevPairs
             , _reportTitle :: Markup
             , _reportHeader :: Markup
             , _reportFooter :: Markup
             , _reportIntendedUse :: Markup
             , _reportIntendedUsers :: Markup
             , _reportValueTypeInfo :: ReportValueTypeInfo
             , _reportValueApproachInfo :: ReportValueApproachInfo
             , _reportClientName :: Markup
             , _reportClientAddress :: Markup
             , _reportClientGreeting :: Markup
             , _reportItemsOwnerFull :: Markup
             , _reportItemsOwner :: Markup
             , _reportBriefItems :: Markup
             , _reportInspectionLocation :: Markup
             , _reportBody :: ReportElems
             , _reportGlossary :: MarkupPairs
             , _reportSources :: MarkupPairs
             , _reportLetterOfTransmittal :: Markup
             , _reportScopeOfWork :: Markup
             , _reportCertification :: Markup
             , _reportLimitingConditions :: Markups
             , _reportPrivacyPolicy :: Markup
             , _reportPerms :: Permissions
             , _reportRevision :: Integer
             , _reportCreated :: Zulu -- Changed from UTCTime in version 25
             , _reportBranding :: Branding
             , _reportStatus :: ReportStatus
             , _reportRedacted :: Bool
             , _reportFlags :: ReportFlags
             , _reportUUID :: ReportID
             , _reportOrderByItemName :: Bool
             , _reportDisplayItemName :: Bool
             , _reportStandardsVersion :: ReportStandard
             , _reportImagePool :: ReportImages
             , _reportTags :: Set Tag
             , _reportPDFParameters :: ReportPDFParameters -- New in version 26
             }
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

type EpochMilli = Int64
type MarkupPair = (Markup, Markup)
type AbbrevPair = (CIString, Markup)
type AbbrevPairs = Order AbbrevPairID AbbrevPair
newtype MarkupID = MarkupID {unMarkupID :: IntJS} deriving (Generic, Eq, Ord, Read, Show, Data, Typeable)
instance Enum MarkupID where
    toEnum = (MarkupID . toEnum)
    fromEnum = (fromEnum . unMarkupID)
newtype MarkupPairID = MarkupPairID {unMarkupPairID :: IntJS} deriving (Generic, Eq, Ord, Read, Show, Data, Typeable)
instance Enum MarkupPairID where
    toEnum = (MarkupPairID . toEnum)
    fromEnum = (fromEnum . unMarkupPairID)
type MarkupPairs = Order MarkupPairID MarkupPair
data Tagged = Digits Text | Chars Text
data ReportValueApproachInfo
    = ReportValueApproachInfo
    { reportValueApproachName :: Markup
    , reportValueApproachDescription :: Markup
    } deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
data ReportValueTypeInfo
    = ReportValueTypeInfo
      { reportValueTypeName :: Markup
      , reportValueTypeDescription :: Markup
      , reportValueTypeDefinition :: Markup
      } deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

newtype AuthorID = AuthorID {unAuthorID :: IntJS} deriving (Generic, Eq, Ord, Read, Show, Data, Typeable)
instance Enum AuthorID where
    toEnum = (AuthorID . toEnum)
    fromEnum = (fromEnum . unAuthorID)
type Authors = Order AuthorID Author

data Author
    = Author
      { authorName :: Markup
      , authorCredentials :: Markup
      , authorId :: Maybe UserId -- There *may* still be appraisers without account
      , authorSig :: Branding
      } deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

data Markup
    = Markdown {markdownText :: Text}
    | Html {htmlText :: Text}
    | LaTeX LaTeX
    | Pandoc P.Pandoc
    | Markup [Markup]
    deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

data ReportPDFParameters
  = ReportPDFParameters
    { _letterOfTransmittalTopSkip :: Float
    }
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

data ReportStatus
    = Draft
    -- ^ This is the current authoritative version of the report
    | Final
    -- ^ The report has been downloaded, and perhaps uploaded to a
    -- different server.
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data, Enum, Bounded)

newtype Tag =
    Tag
    { _tagUUID :: UUID -- ^ A unique permenant identifier for the tag
    } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)
#endif

data ReportMap report = ReportMap {_unReportMap :: Map ReportID (History AppraisalPaths report)} deriving Data

data AppraisalPaths = AppraisalPaths deriving (Generic, Data, Eq, Ord, Show)

data AppraisalData =
    AppraisalData
    { _reportMap :: ReportMap Report
    , _trashCan :: ReportMap Report
    } deriving Data

data ReportElem
    = ReportItem {elemItem :: Item}
    | ReportParagraph {elemText :: Markup}
    | ReportUndecided
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
newtype ReportElemID = ReportElemID {unReportElemID :: IntJS} deriving (Generic, Eq, Ord, Read, Show, Data, Typeable)
instance Enum ReportElemID where
    toEnum = (ReportElemID . toEnum)
    fromEnum = (fromEnum . unReportElemID)
type ReportElems = Order ReportElemID ReportElem
data Permissions = Permissions
    { _owner :: UserId
    , _writers :: UserGroup
    , _readers :: UserGroup
    } deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
data ImageFile
    = ImageFile
      { _imageFile :: File
      , _imageFileType :: ImageType
      , _imageFileWidth :: Int
      , _imageFileHeight :: Int
      , _imageFileMaxVal :: Int
      } deriving (Generic, Show, Read, Eq, Ord, Data, Typeable)
data ImageType = PPM | JPEG | GIF | PNG deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)
data FileSource
    = TheURI String
    | ThePath FilePath
    deriving (Generic, Show, Read, Eq, Ord, Data, Typeable)

type Checksum = String

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: String                  -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Show, Read, Eq, Ord, Data, Typeable)
type Markups = Order MarkupID Markup
data ReportFlags = ReportFlags {
      _hideEmptyItemFields :: Bool
    , _pdfGenerator :: PDFGenerator
    , _reportParSkip :: Measure -- ^ Inter-paragraph vertical skip, default 7.2pt (1mm)
    , _extraTOCEntries :: Bool -- ^ Add a "Title Page" and "Table of Contents" entries to the table of contents
  } deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
data PDFGenerator = Standard | Beta deriving (Generic, Read, Show, Typeable, Data, Eq, Ord, Enum, Bounded)
data ReportStandard = ReportStandard {unReportStandard :: Int} deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
type ReportImages = Order ReportImageID ReportImage
newtype CIString = CIString {unCIString :: String} deriving (Generic, Data, Typeable, Read, Show, Ord, Eq)
newtype AbbrevPairID = AbbrevPairID {unAbbrevPairID :: IntJS} deriving (Generic, Eq, Ord, Read, Show, Data, Typeable)
instance Enum AbbrevPairID where
    toEnum = (AbbrevPairID . toEnum)
    fromEnum = (fromEnum . unAbbrevPairID)
data Item
    = Item { itemName :: Text
           , fields :: MIM -- Map ItemFieldName Markup
           , images :: ReportImages -- Use a typedef here so that paths like Path_Report ReportImages are generated
           } deriving (Generic, Show, Read, Eq, Ord, Data, Typeable)
type MIM = Map ItemFieldName Markup
data ItemFieldName
    = ItemLocation
    | ItemDataSheetNumber
    | ItemTypeOfObject
    | ItemArtistOrMaker
    | ItemArtistDate
    | ItemCountryOrNationality
    | ItemSubjectOrTitle
    | ItemSignatureInscriptionsMarkings
    | ItemMediumMaterialsTechniques
    | ItemDateOrPeriod
    | ItemSupportBaseFrame
    | ItemFrameMeasurement
    | ItemItemMeasurement
    | ItemProvenance
    | ItemCost
    | ItemCondition
    | ItemEdition
    | ItemDescription
    | ItemMarketAnalysis
    | ItemExhibitionsAndPublications
    | ItemAdditionalNotes
    | ItemCashValue
    deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable)
data UserGroup = UserGroup
    { _public :: Bool
    , _users :: Set UserId
    } deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
newtype ReportImageID = ReportImageID {unReportImageID :: IntJS} deriving (Generic, Eq, Ord, Read, Show, Data, Typeable)
instance Enum ReportImageID where
  toEnum = ReportImageID . toEnum
  fromEnum = fromEnum . unReportImageID
data ReportImage
    = ReportImage
      { _picSize :: SaneSize ImageSize
      , _picCrop :: ImageCrop
      , _picCaption :: Markup
      , _picOriginal :: MEUI -- ^ Original image
      , _picMustEnlarge :: Bool        -- ^ Put an enlargement of this image in the appendix
      , _picPropertyType :: PropertyType -- ^ Is this the subject property or a comparable?
      }
    deriving (Generic, Eq, Ord, Show, Read, Data, Typeable)
newtype SaneSize a = SaneSize {unSaneSize :: a} deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
data ImageSize
    = ImageSize
      { dim :: Dimension
      , size :: Rational
      , units :: Units
      } deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)
data Dimension
    = TheHeight
    | TheWidth
    | TheArea
    deriving (Generic, Show, Read, Eq, Ord, Typeable, Data, Enum, Bounded)
data Units
    = Inches
    | Cm
    | Points
    deriving (Generic, Show, Read, Eq, Ord, Typeable, Data, Enum, Bounded)

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Int         -- 0, 90, 180, 270
      } deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)
type MEUI = Maybe EUI
type EUI = Either URI ImageFile
data PropertyType = Subject | Comparable
  deriving (Generic, Eq, Ord, Show, Read, Data, Typeable, Enum, Bounded)
