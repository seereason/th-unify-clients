{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}

import Data.Map
--import Data.Generics hiding (Generic)
--import Data.ListLike (fromListLike)
import Data.SafeCopy
--import Data.Sequence
--import Data.Serialize
--import Data.Serialize.Get
import Data.THUnify.Traverse (pprint1)
import Data.THUnify.SafeCopy (safeCopyInstance)
import Language.Haskell.TH.Lift (lift)
import Test.HUnit
import TestTypes

main :: IO ()
main = do
  cts <- runTestTT tests
  case cts of
    Counts {errors = 0, failures = 0} -> pure ()
    _ -> error (showCounts cts)

tests :: Test
tests = TestList
    [ TestCase (assertEqual "SafeCopy SerializedIndex"
                  (mconcat
                   ["instance SafeCopy SerializedIndex where",
                    " putCopy (SerializedIndex a1) = contain (do {safePut_ByteString <- getSafePut; safePut_ByteString a1; return ()})",
                    " getCopy = contain (label \"SerializedIndex:\" (do {safeGet_ByteString <- getSafeGet; return SerializedIndex <*> safeGet_ByteString}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"SerializedIndex\""])
                  $(safeCopyInstance 1 'base [t|SerializedIndex|] >>= lift . pprint1))
    , TestCase (assertEqual "SafeCopy Hop"
                  (mconcat
                   ["instance SafeCopy key => SafeCopy (Hop key) where",
                    " putCopy (FieldHop a1 a2) = contain (do {putWord8 0; safePut_Int <- getSafePut; safePut_Int a1; safePut_Int a2; return ()})",
                    " putCopy (IxHop a1) = contain (do {putWord8 1; safePut_key <- getSafePut; safePut_key a1; return ()})",
                    " putCopy (AtHop a1) = contain (do {putWord8 2; safePut_key <- getSafePut; safePut_key a1; return ()})",
                    " putCopy (ViewHop) = contain (do {putWord8 3; return ()})",
                    " getCopy = contain (label \"Hop key:\" (do {tag <- getWord8; case tag of 0 -> do {safeGet_Int <- getSafeGet; (return FieldHop <*> safeGet_Int) <*> safeGet_Int} 1 -> do {safeGet_key <- getSafeGet; return IxHop <*> safeGet_key} 2 -> do {safeGet_key <- getSafeGet; return AtHop <*> safeGet_key} 3 -> do return ViewHop _ -> fail (\"Could not identify tag \\\"\" ++ (show tag ++ \"\\\" for type \\\"Hop key\\\" that has only 4 constructors.  Maybe your data is corrupted?\"))}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"Hop key\""])
                  $(safeCopyInstance 1 'base [t|Hop|] >>= lift . pprint1))
    , TestCase (assertEqual "SafeCopy TypePath"
                  (mconcat
#if 1
                   ["instance SafeCopy (TypePath t s a) where",
                    " putCopy (TypePath a1) = contain (do {safePut_ListHopSerializedIndex <- getSafePut; safePut_ListHopSerializedIndex a1; return ()})",
                    " getCopy = contain (label \"TypePath t s a:\" (do {safeGet_ListHopSerializedIndex <- getSafeGet; return TypePath <*> safeGet_ListHopSerializedIndex}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"TypePath t s a\""]
#else
                   ["instance SafeCopy (Path t1 (Proxy s1) (Proxy a1)) where",
                    " putCopy (Path a1 a2 a3) = contain (do {safePut_Proxys1 <- getSafePut; safePut_ListHopSerializedIndex <- getSafePut; safePut_Proxya1 <- getSafePut; safePut_Proxys1 a1; safePut_ListHopSerializedIndex a2; safePut_Proxya1 a3; return ()})",
                    " getCopy = contain (label \"TypePath t1 s1 a1:\" (do {safeGet_Proxys1 <- getSafeGet; safeGet_ListHopSerializedIndex <- getSafeGet; safeGet_Proxya1 <- getSafeGet; ((return Path <*> safeGet_Proxys1) <*> safeGet_ListHopSerializedIndex) <*> safeGet_Proxya1}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"TypePath t1 s1 a1\""]
#endif
                  )
                  $(safeCopyInstance 1 'base [t|TypePath|] >>= lift . pprint1))
    , TestCase (assertEqual "SafeCopy TypeSPath"
                  (mconcat
#if 1
                   ["instance SafeCopy (TypeSPath t s) where",
                    " putCopy (TypeSPath a1) = contain (do {safePut_ListHopSerializedIndex <- getSafePut; safePut_ListHopSerializedIndex a1; return ()})",
                    " getCopy = contain (label \"TypeSPath t s:\" (do {safeGet_ListHopSerializedIndex <- getSafeGet; return TypeSPath <*> safeGet_ListHopSerializedIndex}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"TypeSPath t s\""]
#else
                   ["instance SafeCopy (Path t1 (Proxy s1) ()) where",
                    " putCopy (Path a1 a2 a3) = contain (do {safePut_Proxys1 <- getSafePut; safePut_ListHopSerializedIndex <- getSafePut; safePut_Tuple0 <- getSafePut; safePut_Proxys1 a1; safePut_ListHopSerializedIndex a2; safePut_Tuple0 a3; return ()})",
                    " getCopy = contain (label \"TypeSPath t1 s1:\" (do {safeGet_Proxys1 <- getSafeGet; safeGet_ListHopSerializedIndex <- getSafeGet; safeGet_Tuple0 <- getSafeGet; ((return Path <*> safeGet_Proxys1) <*> safeGet_ListHopSerializedIndex) <*> safeGet_Tuple0}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"TypeSPath t1 s1\""]
#endif
                  )
                  $(safeCopyInstance 1 'base [t|TypeSPath|] >>= lift . pprint1))
    , TestCase (assertEqual "SafeCopy PathValue"
                 (mconcat
#if 1
                   ["instance SafeCopy (PathValue t3 s3) where",
                    " putCopy (PathValue a1 a2) = contain (do {safePut_TypeSPatht3s3 <- getSafePut; safePut_ByteString <- getSafePut; safePut_TypeSPatht3s3 a1; safePut_ByteString a2; return ()})",
                    " getCopy = contain (label \"PathValue t3 s3:\" (do {safeGet_TypeSPatht3s3 <- getSafeGet; safeGet_ByteString <- getSafeGet; (return PathValue <*> safeGet_TypeSPatht3s3) <*> safeGet_ByteString}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"PathValue t3 s3\""]
#else
                   ["instance SafeCopy (PathValue t3 s3) where",
                    " putCopy (PathValue a1 a2) = contain (do {safePut_Patht1Proxys1Tuple0t3s3 <- getSafePut; safePut_ByteString <- getSafePut; safePut_Patht1Proxys1Tuple0t3s3 a1; safePut_ByteString a2; return ()})",
                    " getCopy = contain (label \"PathValue t3 s3:\" (do {safeGet_Patht1Proxys1Tuple0t3s3 <- getSafeGet; safeGet_ByteString <- getSafeGet; (return PathValue <*> safeGet_Patht1Proxys1Tuple0t3s3) <*> safeGet_ByteString}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"PathValue t3 s3\""]
#endif
                  )
                  $(safeCopyInstance 1 'base [t|PathValue|] >>= lift . pprint1))
    , TestCase (assertEqual "SafeCopy Either"
                  (mconcat
                   ["instance (SafeCopy k, SafeCopy a) => SafeCopy (Map k a) where",
                    " putCopy (Bin a1 a2 a3 a4 a5) = contain (do {putWord8 0; safePut_Int <- getSafePut; safePut_k <- getSafePut; safePut_a <- getSafePut; safePut_Mapka <- getSafePut; safePut_Int a1; safePut_k a2; safePut_a a3; safePut_Mapka a4; safePut_Mapka a5; return ()})",
                    " putCopy (Tip) = contain (do {putWord8 1; return ()})",
                    " getCopy = contain (label \"Map k a:\" (do {tag <- getWord8; case tag of 0 -> do {safeGet_Int <- getSafeGet; safeGet_k <- getSafeGet; safeGet_a <- getSafeGet; safeGet_Mapka <- getSafeGet; ((((return Bin <*> safeGet_Int) <*> safeGet_k) <*> safeGet_a) <*> safeGet_Mapka) <*> safeGet_Mapka} 1 -> do return Tip _ -> fail (\"Could not identify tag \\\"\" ++ (show tag ++ \"\\\" for type \\\"Map k a\\\" that has only 2 constructors.  Maybe your data is corrupted?\"))}))",
                    " version = 1",
                    " kind = base",
                    " errorTypeName _ = \"Map k a\""])
                  $(safeCopyInstance 1 'base [t|Map|] >>= lift . pprint1))
    ]

-- These just need to compile.  There are cases above to check
-- the actual template haskell output, but unless you actually
-- compile them you don't know if something in the environment
-- might have made them stop working.

$(safeCopyInstance 1 'base [t|SerializedIndex|])
$(safeCopyInstance 1 'base [t|SerializedIxValue|])
$(safeCopyInstance 1 'base [t|SerializedValue|])
$(safeCopyInstance 1 'base [t|Hop|])
-- $(safeCopyInstance 1 'base [t|Hop SerializedIndex|])
$(safeCopyInstance 5 'base [t|TypePath|])
$(safeCopyInstance 5 'base [t|TypeSPath|])
$(safeCopyInstance 5 'base [t|TypeEPath|])
$(safeCopyInstance 5 'base [t|TypeUPath|])
$(safeCopyInstance 1 'base [t|PathValue|])

$(safeCopyInstance 1 'base [t|PathError|])
instance (SafeCopy' k, SafeCopy' v, SafeCopy' a, SafeCopy' t, SafeCopy' s) => SafeCopy (Op k v a t s) where version = 3
instance (SafeCopy' k, SafeCopy' v, SafeCopy' a, SafeCopy' t, SafeCopy' s) => SafeCopy (Edit k v a t s) where version = 2
instance (SafeCopy' k, SafeCopy' v, SafeCopy' a, SafeCopy' t, SafeCopy' s) => SafeCopy (EditError k v a t s) where version = 2; errorTypeName _ = "EditError"
$(safeCopyInstance 2 'base [t|EventId|])
instance (SafeCopy' k, SafeCopy' v, SafeCopy' a, SafeCopy' t, SafeCopy' s) => SafeCopy (Event k v a t s) where version = 4
instance (SafeCopy' t, SafeCopy' s) => SafeCopy (EventTree t s) where version = 3
instance (SafeCopy' t, SafeCopy' s) => SafeCopy (HistoryTree_1 t s)
-- Because Order_0 has a deriving Data instance, and that Data
-- instance has an Ord k constraint, this SafeCopy instance also needs
-- an Ord constraint on k.  So we need to collect the constraints on
-- the SafeCopy instances of all the subtypes.
-- $(safeCopyInstance 0 'base [t|Order_0|])
-- $(safeCopyInstance 1 'extension [t|VMap|])

instance (SafeCopy' t, SafeCopy' s) => SafeCopy (History t s)
$(safeCopyInstance 1 'base [t|ReportID|])
-- $(safeCopyInstance 1 'base [t|Report|])
-- $(safeCopyInstance 1 'base [t|ReportMap|])
-- $(safeCopyInstance 1 'base [t|AppraisalData|])
