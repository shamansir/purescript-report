module Report.Decorator where

import Prelude

import Foreign (Foreign, F)

import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes, elemIndex) as Array
import Data.String (Pattern(..), stripPrefix) as String

import Report.Prefix (Prefix)
import Report.Suffix (Suffix)

import Report.Core as CT
import Report.GroupPath (GroupPath) as GP
import Report.Modifiers (Modifiers) as R
import Report.Modifiers.Progress as P
import Report.Modifiers.Priority (Priority)
import Report.Modifiers.Rating (Rating)
import Report.Modifiers.Task (TaskP)
import Report.Modifiers.Tags (Tags)
import Report.Convert.Keyed as CK

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)



data Decorator t
    = PRating Rating
    | PPriority Priority
    | PTask TaskP
    | SProgress P.Progress
    | SEarnedAt CT.SDate
    | SDescription String
    | SReference GP.GroupPath
    | STags (Tags t)



-- derive instance Eq (Decorator t)
-- instance Ord t => Ord (Decorator t) where
--     compare a b = CK.keyOf @Key a `compare` CK.keyOf @Key b

derive instance Eq Key

instance Ord Key where
    compare a b = orderOf a `compare` orderOf b


data Key
    = KRating
    | KPriority
    | KTask
    | KProgress P.PValueTag
    | KEarnedAt
    | KDescription
    | KReference
    | KTags


instance CK.Keyed Key (Decorator t) where
    keyOf = case _ of
        PRating _ -> KRating
        PPriority _ -> KPriority
        PTask _ -> KTask
        SProgress prog -> KProgress $ CK.keyOf prog
        SEarnedAt _ -> KEarnedAt
        SDescription _ -> KDescription
        SReference _ -> KReference
        STags _ -> KTags



newtype Decorators t = Decorators (Map Key (Decorator t)) -- other names: `Adornments`, `Markers`, `Attributes, `Modifiers`
derive instance Newtype (Decorators t) _


instance ReadForeign (Decorators String)
    where
        readImpl frgn = do
            recArr <- (readImpl frgn :: F (Array { key :: String, value :: Decorator String }))
            let tupleArr = recArr <#> (\{ key, value } -> decodeKey key <#> \k -> k /\ value)
            pure $ fromArray $ Array.catMaybes tupleArr
instance WriteForeign (Decorators String)
    where
        writeImpl modifiers =
            let recArr = toArray modifiers <#> \(k /\ v) -> { key: encodeKey k, value: v }
            in  writeImpl recArr


empty :: forall tag. Decorators tag
empty = wrap Map.empty


size :: forall tag. Decorators tag -> Int
size = unwrap >>> Map.size


get :: forall tag. Key -> Decorators tag -> Maybe (Decorator tag)
get key = unwrap >>> Map.lookup key


keys :: forall tag. Decorators tag -> Array Key
keys = unwrap >>> Map.keys >>> Set.toUnfoldable


put :: forall tag. Decorator tag -> Decorators tag -> Decorators tag
put modifier = unwrap >>> Map.insert (CK.keyOf modifier) modifier >>> wrap


toArray :: forall tag. Decorators tag -> Array (Key /\ Decorator tag)
toArray = unwrap >>> Map.toUnfoldable


fromArray :: forall tag. Ord Key => Array (Key /\ Decorator tag) -> Decorators tag
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


decodeWithKey :: forall tag. ReadForeign tag => Key -> Foreign -> F (Decorator tag)
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


instance CK.DecodeKeyed Key (Decorator String) where
    toValue = decodeWithKey


instance ReadForeign (Decorator String) where
    readImpl frgn = CK.decodeKeyed @Key =<< (readImpl frgn :: F CK.JsonTM)


instance WriteForeign (Decorator String) where
    writeImpl anyOf = writeImpl $ CK.encodeKeyed @Key anyOf


isPrefix :: Key -> Boolean
isPrefix = case _ of
    KRating -> true
    KPriority -> true
    KTask -> true
    _ -> false



isSuffix :: Key -> Boolean
isSuffix = case _ of
    KProgress _ -> true
    KEarnedAt -> true
    KDescription -> true
    KReference -> true
    KTags -> true
    _ -> false


prefixesInOrder :: Array Key
prefixesInOrder = [ KRating, KPriority, KTask ]


suffixesInOrder :: Array Key
suffixesInOrder = (P.inlineProgress <#> KProgress) <> [ KEarnedAt, KDescription, KReference, KTags ] <> (P.leveledProgress <#> KProgress)


orderOf :: Key -> Int
orderOf = case _ of
    KRating -> -3
    KPriority -> -2
    KTask -> -1
    KProgress (P.PValueTag pvt) ->
        case pvt of
            "I" -> 0
            "N" -> 1
            "T" -> 2
            "D" -> 3
            "PCTI" -> 4
            "PCT" -> 5
            "PCTX" -> 6
            "PI" -> 7
            "PD" -> 8
            "TIME" -> 9
            "DATE" -> 10
            "PERI" -> 11
            "PERD" -> 12
            "MESI" -> 13
            "MESD" -> 14
            "MESX" -> 15
            "RNGI" -> 16
            "RNGD" -> 17
            "PROC" -> 18
            "RELT" -> 19
            "X" -> 20

            "LVLI" -> levelShift + 0
            "LVLD" -> levelShift + 1
            "LVLO" -> levelShift + 2
            "LVLS" -> levelShift + 3
            "LVLP" -> levelShift + 4
            "LVLC" -> levelShift + 5
            "LVLE" -> levelShift + 6

            _ -> 1001

    KEarnedAt -> 21
    KDescription -> 22
    KReference -> 23
    KTags -> 24
    where
        levelShift = 25



allKeys :: Array Key
allKeys =
    suffixesInOrder <> suffixesInOrder