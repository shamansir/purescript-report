module Report.Decorator where

import Prelude

import Foreign (Foreign, F)

import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes, elemIndex, filter, findMap) as Array
import Data.String (Pattern(..), stripPrefix) as String

import Report.Core as CT
import Report.GroupPath (GroupPath) as GP
import Report.Decorators.Progress as P
import Report.Decorators.Priority (Priority)
import Report.Decorators.Rating (Rating)
import Report.Decorators.Task (TaskP)
import Report.Decorators.Tags (Tags, RawTag(..))
import Report.Convert.Keyed as CK

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)



data Decorator
    = PRating Rating
    | PPriority Priority
    | PTask TaskP
    | SProgress P.Progress
    | SEarnedAt CT.SDate
    | SDescription String
    | SReference GP.GroupPath
    -- | STags (Tags t)


-- derive instance Eq (Decorator t)
-- instance Ord t => Ord (Decorator t) where
--     compare a b = CK.keyOf @Key a `compare` CK.keyOf @Key b

derive instance Eq Key

instance Ord Key where
    compare a b = orderOf a `compare` orderOf b


data Key
    = KRating
    | KPriority
    | KTask -- FIXME: KProgress can contain Task
    | KProgress P.PValueTag
    | KEarnedAt
    | KDescription
    | KReference


instance CK.Keyed Key Decorator where
    keyOf = case _ of
        PRating _ -> KRating
        PPriority _ -> KPriority
        PTask _ -> KTask
        SProgress prog -> KProgress $ CK.keyOf prog
        SEarnedAt _ -> KEarnedAt
        SDescription _ -> KDescription
        SReference _ -> KReference


newtype Decorators = Decorators (Map Key Decorator) -- other names: `Adornments`, `Markers`, `Attributes, `Modifiers`
derive instance Newtype Decorators _


instance ReadForeign Decorators
    where
        readImpl frgn = do
            recArr <- (readImpl frgn :: F (Array { key :: String, value :: Decorator }))
            let tupleArr = recArr <#> (\{ key, value } -> decodeKey key <#> \k -> k /\ value)
            pure $ fromArray $ Array.catMaybes tupleArr
instance WriteForeign Decorators
    where
        writeImpl modifiers =
            let recArr = toArray modifiers <#> \(k /\ v) -> { key: encodeKey k, value: v }
            in  writeImpl recArr


empty :: Decorators
empty = wrap Map.empty


size :: Decorators -> Int
size = unwrap >>> Map.size


get :: Key -> Decorators -> Maybe Decorator
get key = unwrap >>> Map.lookup key


keys :: Decorators -> Array Key
keys = unwrap >>> Map.keys >>> Set.toUnfoldable


put :: Decorator -> Decorators -> Decorators
put modifier = unwrap >>> Map.insert (CK.keyOf modifier) modifier >>> wrap


toArray :: Decorators -> Array (Key /\ Decorator)
toArray = unwrap >>> Map.toUnfoldable


fromArray :: Ord Key => Array (Key /\ Decorator) -> Decorators
fromArray = wrap <<< Map.fromFoldable


prefixes :: Decorators -> Array (Key /\ Decorator)
prefixes = unwrap >>> Map.toUnfoldable >>> Array.filter (isPrefix <<< Tuple.fst)


suffixes :: Decorators -> Array (Key /\ Decorator)
suffixes = unwrap >>> Map.toUnfoldable >>> Array.filter (isSuffix <<< Tuple.fst)


encodeKey :: Key -> String
encodeKey = case _ of
    KRating -> "RATING"
    KPriority -> "PRIORITY"
    KTask -> "TASK"
    KProgress pvtag -> "PROG:" <> CK.encodeKey @P.PValueTag pvtag
    KEarnedAt -> "EARN"
    KDescription -> "DESC"
    KReference -> "REF"


decodeKey :: String -> Maybe Key
decodeKey str =
    case String.stripPrefix (String.Pattern "PROG:") str of
        Just pvtag -> (CK.decodeKey pvtag :: Maybe P.PValueTag) <#> KProgress
        Nothing -> case str of
            "PROG" -> Just $ KProgress $ P.PValueTag "UNK" -- FIXME
            "EARN" -> Just KEarnedAt
            "DESC" -> Just KDescription
            "REF"  -> Just KReference
            "RATING"   -> Just KRating
            "PRIORITY" -> Just KPriority
            "TASK"     -> Just KTask
            _      -> Nothing


decodeWithKey :: Key -> Foreign -> F Decorator
decodeWithKey key f = case key of
    KRating   -> PRating   <$> readImpl f
    KPriority -> PPriority <$> readImpl f
    KTask     -> PTask     <$> readImpl f
    KProgress _ -> SProgress <$> readImpl f
    KEarnedAt -> SEarnedAt <$> readImpl f
    KDescription -> SDescription <$> readImpl f
    KReference -> SReference <$> readImpl f


writeValue :: Decorator -> Foreign
writeValue = case _ of
    PRating r -> writeImpl r
    PPriority p -> writeImpl p
    PTask t -> writeImpl t
    SProgress prog -> writeImpl prog
    SEarnedAt d -> writeImpl d
    SDescription desc -> writeImpl desc
    SReference path -> writeImpl path


debugNavLabel :: Key -> String
debugNavLabel = encodeKey


instance CK.EncodableKey Key where
    encodeKey = encodeKey
    decodeKey = decodeKey


instance CK.KeyedReadForeign Key Decorator where
    keyedReadImpl = decodeWithKey


instance ReadForeign Decorator where
    readImpl frgn =
        (CK.readImpl' frgn :: F (CK.KeyedValue Key Foreign))
            >>= \kv -> decodeWithKey (CK.key kv) (CK.value kv)


instance WriteForeign Decorator where
    writeImpl decorator =
        CK.writeImpl' $ CK.make (CK.keyOf @Key decorator) $ writeValue decorator


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
    _ -> false


prefixesInOrder :: Array Key
prefixesInOrder = [ KRating, KPriority, KTask ]


suffixesInOrder :: Array Key
suffixesInOrder = (P.inlineProgress <#> KProgress) <> [ KEarnedAt, KDescription, KReference ] <> (P.leveledProgress <#> KProgress)


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
    -- KTags -> 24
    where
        levelShift = 25



allKeys :: Array Key
allKeys =
    prefixesInOrder <> suffixesInOrder


getProgress :: P.PValueTag -> Decorators -> Maybe P.Progress
getProgress pvtag dcrts = get (KProgress pvtag) dcrts >>= case _ of
    SProgress p -> Just p
    _ -> Nothing


hasProgress :: Decorators -> Maybe P.PValueTag
hasProgress = keys >>> Array.findMap
    ( case _ of
        KProgress p -> Just p
        _ -> Nothing
    )


collectProgress :: Decorators -> Array (Maybe P.PValueTag /\ P.Progress)
collectProgress sfx =
    toArray sfx
        <#> (\(key /\ suffix) -> case key /\ suffix of
                KProgress pvtag /\ SProgress p -> Just $ Just pvtag /\ p
                _ /\ SProgress p -> Just $ Nothing /\ p
                _ -> Nothing
            )
         # Array.catMaybes


getEarnedAt :: Decorators -> Maybe CT.SDate
getEarnedAt sfx = get KEarnedAt sfx >>= case _ of
    SEarnedAt d -> Just d
    _ -> Nothing


getDescription :: Decorators -> Maybe String
getDescription sfx = get KDescription sfx >>= case _ of
    SDescription desc -> Just desc
    _ -> Nothing


getReference :: Decorators -> Maybe GP.GroupPath
getReference sfx = get KReference sfx >>= case _ of
    SReference path -> Just path
    _ -> Nothing


getRating :: Decorators -> Maybe Rating
getRating pfx = get KRating pfx >>= case _ of
    PRating r -> Just r
    _ -> Nothing


getPriotity :: Decorators -> Maybe Priority
getPriotity pfx = get KPriority pfx >>= case _ of
    PPriority p -> Just p
    _ -> Nothing


getTask :: Decorators ->  Maybe TaskP
getTask pfx = get KTask pfx >>= case _ of
    PTask t -> Just t
    _ -> Nothing