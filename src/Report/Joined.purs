module Report.Joined where

import Prelude

import Foreign (Foreign, F)

import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes) as Array
import Data.String (Pattern(..), stripPrefix) as String

import Report.Prefix (Prefix)
import Report.Suffix (Suffix)

import Report.Core as CT
import Report.GroupPath (GroupPath) as GP
import Report.Modifiers (Modifiers) as R
import Report.Modifiers.Progress (Progress, PValueTag(..)) as P
import Report.Modifiers.Priority (Priority)
import Report.Modifiers.Rating (Rating)
import Report.Modifiers.Task (TaskP)
import Report.Modifiers.Tags (Tags)
import Report.Convert.Keyed as CK

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)



data AnyOf t
    = PRating Rating
    | PPriority Priority
    | PTask TaskP
    | SProgress P.Progress
    | SEarnedAt CT.SDate
    | SDescription String
    | SReference GP.GroupPath
    | STags (Tags t)



-- derive instance Eq (AnyOf t)
-- instance Ord t => Ord (AnyOf t) where
--     compare a b = CK.keyOf @Key a `compare` CK.keyOf @Key b

derive instance Eq Key
derive instance Ord Key

-- instance Ord Key where
--     compare a b = orderIndex a `compare` orderIndex b


data Key
    = KRating
    | KPriority
    | KTask
    | KProgress P.PValueTag
    | KEarnedAt
    | KDescription
    | KReference
    | KTags


instance CK.Keyed Key (AnyOf a) where
    keyOf = case _ of
        PRating _ -> KRating
        PPriority _ -> KPriority
        PTask _ -> KTask
        SProgress prog -> KProgress $ CK.keyOf prog
        SEarnedAt _ -> KEarnedAt
        SDescription _ -> KDescription
        SReference _ -> KReference
        STags _ -> KTags



newtype TheModifiers t = TheModifiers (Map Key (AnyOf t))
derive instance Newtype (TheModifiers t) _


instance ReadForeign (TheModifiers String)
    where
        readImpl frgn = do
            recArr <- (readImpl frgn :: F (Array { key :: String, value :: AnyOf String }))
            let tupleArr = recArr <#> (\{ key, value } -> decodeKey key <#> \k -> k /\ value)
            pure $ fromArray $ Array.catMaybes tupleArr
instance WriteForeign (TheModifiers String)
    where
        writeImpl modifiers =
            let recArr = toArray modifiers <#> \(k /\ v) -> { key: encodeKey k, value: v }
            in  writeImpl recArr


empty :: forall tag. TheModifiers tag
empty = wrap Map.empty


size :: forall tag. TheModifiers tag -> Int
size = unwrap >>> Map.size


get :: forall tag. Key -> TheModifiers tag -> Maybe (AnyOf tag)
get key = unwrap >>> Map.lookup key


keys :: forall tag. TheModifiers tag -> Array Key
keys = unwrap >>> Map.keys >>> Set.toUnfoldable


put :: forall tag. AnyOf tag -> TheModifiers tag -> TheModifiers tag
put modifier = unwrap >>> Map.insert (CK.keyOf modifier) modifier >>> wrap


toArray :: forall tag. TheModifiers tag -> Array (Key /\ AnyOf tag)
toArray = unwrap >>> Map.toUnfoldable


fromArray :: forall tag. Ord Key => Array (Key /\ AnyOf tag) -> TheModifiers tag
fromArray = wrap <<< Map.fromFoldable


encodeKey :: Key -> String
encodeKey = case _ of
    KRating -> "RATING"
    KPriority -> "PRIORITY"
    KTask -> "TASK"
    KProgress pvtag -> "PROG:" <> CK.encodeKey @P.PValueTag pvtag
    KEarnedAt -> "EARN"
    KDescription -> "DESC"
    KReference -> "REF"
    KTags -> "TAGS"


decodeKey :: String -> Maybe Key
decodeKey str =
    case String.stripPrefix (String.Pattern "PROG:") str of
        Just pvtag -> (CK.decodeKey pvtag :: Maybe P.PValueTag) <#> KProgress
        Nothing -> case str of
            "PROG" -> Just $ KProgress $ P.PValueTag "UNK" -- FIXME
            "EARN" -> Just KEarnedAt
            "DESC" -> Just KDescription
            "REF"  -> Just KReference
            "TAGS" -> Just KTags
            "RATING"   -> Just KRating
            "PRIORITY" -> Just KPriority
            "TASK"     -> Just KTask
            _      -> Nothing


decodeWithKey :: forall tag. ReadForeign tag => Key -> Foreign -> F (AnyOf tag)
decodeWithKey key f = case key of
    KRating   -> PRating   <$> readImpl f
    KPriority -> PPriority <$> readImpl f
    KTask     -> PTask     <$> readImpl f
    KProgress _ -> SProgress <$> readImpl f
    KEarnedAt -> SEarnedAt <$> readImpl f
    KDescription -> SDescription <$> readImpl f
    KReference -> SReference <$> readImpl f
    KTags -> STags <$> readImpl f


debugNavLabel :: Key -> String
debugNavLabel = encodeKey


instance CK.EncodableKey Key where
    encodeKey = encodeKey
    decodeKey = decodeKey


instance CK.DecodeKeyed Key (AnyOf String) where
    toValue = decodeWithKey


instance ReadForeign (AnyOf String) where
    readImpl frgn = CK.decodeKeyed @Key =<< (readImpl frgn :: F CK.JsonTM)


instance WriteForeign (AnyOf String) where
    writeImpl anyOf = writeImpl $ CK.encodeKeyed @Key anyOf


orderIndex :: Key -> Int
orderIndex = case _ of
    KRating -> -3
    KPriority -> -2
    KTask -> -1
    KProgress _ -> 0
    KEarnedAt -> 1
    KDescription -> 2
    KReference -> 3
    KTags -> 4