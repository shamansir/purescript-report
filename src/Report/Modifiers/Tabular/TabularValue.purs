module Report.Modifiers.Tabular.TabularValue where

import Prelude

import Foreign (Foreign, F)
import Foreign (fail, ForeignError(..)) as F

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

import Report.Core (Year, SDate, SDateRec, STimeRec)
import Report.Prefix (Prefix)
import Report.Suffix (Suffix(..))
import Report.Modifiers.Progress (Progress) as P
import Report.Convert.Keyed as B
import Report.Tabular (class ToTabularValue, Tabular)


data TabularAtomicValue
    = TVString String
    | TVInt Int
    | TVID Int
    | TVYear Int
    | TVNumber Number
    | TVBoolean Boolean
    | TVTime STimeRec
    | TVDate SDate
    | TVDateTime SDate STimeRec
    | TVTimeRange { from :: STimeRec, to :: STimeRec }
    | TVDateRange { from :: SDate,    to :: SDate }
    | TVDateTimeRange
         { from :: { date :: SDate, time :: STimeRec }
         , to   :: { date :: SDate, time :: STimeRec }
         }
    | TVPrefix Prefix
    | TVSuffix (Suffix String)


data TabularValue
    = TVAtomic TabularAtomicValue
    | TVValues (Array TabularAtomicValue)
    | TVValuesNest (Array (Array TabularAtomicValue))
    | TVTabulars (Array (Tabular TabularAtomicValue))
    | TVPair TabularValue TabularValue
    | TVTabularsNest
        { direct :: Array (Tabular TabularAtomicValue)
        , parts :: Array (TabularAtomicValue /\ Array (Tabular TabularAtomicValue))
        }
    -- | TVNest (Array TabularValue)

-- data LIField v
--     = LIField v
--     | LISectionTitle v -- an internal hack
--     | LIValues (Array v)
--     | LIValuesNest (Array (Array v))
--     | LITabulars (Array (Tabular (LIField v)))
--     | LITabularsNest
--         { direct :: Array (Tabular (LIField v))
--         , parts :: Array (v /\ Array (Tabular (LIField v)))
--         }


newtype TVTag = TVTag String
derive instance Newtype TVTag _
derive newtype instance ReadForeign  TVTag
derive newtype instance WriteForeign TVTag


instance ToTabularValue String TabularValue where toTV = Just <<< TVAtomic <<< TVString
instance ToTabularValue Int TabularValue where toTV = Just <<< TVAtomic <<< TVInt
instance ToTabularValue Year TabularValue where toTV = Just <<< TVAtomic <<< TVYear <<< unwrap
instance ToTabularValue Number TabularValue where toTV = Just <<< TVAtomic <<< TVNumber
instance ToTabularValue (Maybe Int) TabularValue where toTV = map (TVAtomic <<< TVInt)
instance ToTabularValue (Maybe Number) TabularValue where toTV = map (TVAtomic <<< TVNumber)
instance ToTabularValue (Maybe String) TabularValue where toTV = map (TVAtomic <<< TVString)
instance ToTabularValue (Maybe Year) TabularValue where toTV = map (TVAtomic <<< TVInt <<< unwrap)



{-
instance ToTabularValue String (LIField String) where toTV s = if String.length s > 0 then Just $ LIField s else Nothing
instance ToTabularValue Boolean (LIField String) where toTV bool = if bool then Just $ LIField "" else Nothing
instance ToTabularValue CSV.QuotedInt (LIField String) where toTV = show >>> LIField >>> Just
instance ToTabularValue T.Completion (LIField String) where toTV = show >>> LIField >>> Just
instance ToTabularValue T.Platform (LIField String) where toTV = show >>> LIField >>> Just
instance ToTabularValue T.PlayStatus (LIField String) where toTV = show >>> LIField >>> Just
instance ToTabularValue T.AuthorsList (LIField String) where toTV alist = if Authors.howMany alist > 0 then Just $ LIField $ show alist else Nothing
instance ToTabularValue T.PlusMinus (LIField String) where toTV = show >>> LIField >>> Just
instance ToTabularValue T.GameRating (LIField String) where toTV = show >>> LIField >>> Just
instance ToTabularValue (Maybe String) (LIField String) where toTV = flip bind Tab.toTV
instance ToTabularValue (Maybe Number) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe Int) (LIField String) where toTV = map (Int.toStringAs Int.decimal >>> LIField)
instance ToTabularValue (Maybe T.YesNo) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe T.Isbn) (LIField String) where toTV = flip bind \(T.Isbn str) -> Tab.toTV str
instance ToTabularValue (Maybe T.Isbn13) (LIField String) where toTV = flip bind \(T.Isbn13 str) -> Tab.toTV str
instance ToTabularValue (Maybe T.CompilationAuthor) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe T.Edition) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe T.Part) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe T.Serie) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Array C.Tag) (LIField String) where toTV tags = if Array.length tags > 0 then Just $ LIField $ String.joinWith ", " $ C.tagToCode <$> tags else Nothing
instance ToTabularValue (Maybe T.PaperFormat) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe T.YearOrTwo) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe CSV.Attributes) (LIField String) where toTV = map (show >>> LIField)
instance ToTabularValue (Maybe T.DiscFormat) (LIField String) where toTV = map (show >>> LIField)
-}



instance B.Keyed TVTag TabularValue where
    keyOf = tagOf


instance B.Keyed TVTag TabularAtomicValue where
    keyOf = tagOfAtomic


instance B.EncodableKey TVTag where
    encodeKey (TVTag str) = str
    decodeKey str = Just $ TVTag str


instance B.DecodeKeyed TVTag TabularAtomicValue where
    toValue = decodeAtomicWithKey


instance B.DecodeKeyed TVTag TabularValue where
    toValue = decodeWithKey


tagOfAtomic :: TabularAtomicValue -> TVTag
tagOfAtomic = TVTag <<< case _ of
    TVString _ -> "S"
    TVInt _ -> "I"
    TVID _ -> "ID"
    TVYear _ -> "Y"
    TVNumber _ -> "N"
    TVBoolean _ -> "B"
    TVTime _ -> "T"
    TVDate _ -> "D"
    TVDateTime _ _ -> "DT"
    TVTimeRange _ -> "TR"
    TVDateRange _ -> "DR"
    TVDateTimeRange _ -> "DTR"
    TVPrefix _ -> "PX"
    TVSuffix _ -> "SX"


tagOf :: TabularValue -> TVTag
tagOf = case _ of
    TVAtomic av -> tagOfAtomic av
    TVValues _ -> TVTag "TV"
    TVValuesNest _ -> TVTag "TVN"
    TVTabulars _ -> TVTag "TVT"
    TVTabularsNest _ -> TVTag "TVTN"
    TVPair _ _ -> TVTag "TVP"
    -- TVNest _ -> "NS"


decodeWithKey :: TVTag -> Foreign -> F TabularValue
decodeWithKey key frgn = case key of
    TVTag "TV" -> TVValues <$> (readImpl frgn :: F (Array TabularAtomicValue))
    TVTag "TVN" -> TVValuesNest <$> (readImpl frgn :: F (Array (Array TabularAtomicValue)))
    TVTag "TVP" -> (Tuple.uncurry TVPair) <$> (readImpl frgn :: F (TabularValue /\ TabularValue))
    _          -> TVAtomic <$> decodeAtomicWithKey key frgn


instance WriteForeign TabularValue where
    writeImpl tabular = writeImpl $ B.make tabularKey $ case tabular of
        TVAtomic av -> writeImpl av
        TVValues avs -> writeImpl avs
        TVValuesNest avsn -> writeImpl avsn
        TVTabulars tbs -> writeImpl tbs
        TVTabularsNest tbsn -> writeImpl tbsn
        TVPair tA tB -> writeImpl (tA /\ tB)
        where
            tabularKey = B.encodeKey $ tagOf tabular

decodeAtomicWithKey :: TVTag -> Foreign -> F TabularAtomicValue
decodeAtomicWithKey key frgn = case key of
    TVTag "S"  -> TVString  <$> (readImpl frgn :: F String)
    TVTag "N"  -> TVNumber  <$> (readImpl frgn :: F Number)
    TVTag "I"  -> TVInt     <$> (readImpl frgn :: F Int)
    TVTag "Y"  -> TVYear    <$> (readImpl frgn :: F Int)
    TVTag "ID" -> TVID      <$> (readImpl frgn :: F Int)
    TVTag "B"  -> TVBoolean <$> (readImpl frgn :: F Boolean)
    TVTag "T"  -> TVTime    <$> (readImpl frgn :: F STimeRec)
    TVTag "D"  -> TVDate    <$> (readImpl frgn :: F SDate)
    TVTag "DT" -> do
        rec <- readImpl frgn :: F { date :: SDate, time :: STimeRec }
        pure $ TVDateTime rec.date rec.time
    TVTag "TR" -> TVTimeRange <$> (readImpl frgn :: F { from :: STimeRec, to :: STimeRec })
    TVTag "DR" -> TVDateRange <$> (readImpl frgn :: F { from :: SDate, to :: SDate })
    TVTag "DTR" -> do
        rec <- readImpl frgn :: F { from :: { date :: SDate, time :: STimeRec }, to :: { date :: SDate, time :: STimeRec } }
        pure $ TVDateTimeRange { from: rec.from, to: rec.to }
    TVTag "PX" -> TVPrefix <$> (readImpl frgn :: F Prefix)
    TVTag "SX" -> TVSuffix <$> (readImpl frgn :: F (Suffix String))
    _          -> F.fail $ F.ForeignError "Unknown TabularAtomicValue tag"


instance ReadForeign TabularValue where
    readImpl frgn = B.decodeKeyed @TVTag =<< (readImpl frgn :: F B.JsonTM)


instance ReadForeign TabularAtomicValue where
    readImpl frgn = B.decodeKeyed @TVTag =<< (readImpl frgn :: F B.JsonTM)


instance WriteForeign TabularAtomicValue where
    -- somehow we cannot use `B.encodeKeyed @Key @(Suffix t) suffix` here, it leads to stack overflow.
    -- writeImpl tabular = -- writeImpl $ encodeKeyed @TVTag tabular
    writeImpl tabular = writeImpl $ B.make tabularKey $ case tabular of
        TVString s -> writeImpl s
        TVNumber n -> writeImpl n
        TVInt n -> writeImpl n
        TVID n -> writeImpl n
        TVYear n -> writeImpl n
        TVBoolean b -> writeImpl b
        TVTime t -> writeImpl t
        TVDate d -> writeImpl d
        TVDateTime d t -> writeImpl { date: d, time: t }
        TVTimeRange tr -> writeImpl tr
        TVDateRange dr -> writeImpl dr
        TVDateTimeRange dtr -> writeImpl dtr
        TVPrefix pfx -> writeImpl pfx
        TVSuffix sfx -> writeImpl sfx
        where
            tabularKey = B.encodeKey $ tagOfAtomic tabular


progress :: P.Progress -> TabularValue
progress = TVAtomic <<< TVSuffix <<< SProgress


suf :: Suffix String -> TabularValue
suf = TVAtomic <<< TVSuffix


pref :: Prefix -> TabularValue
pref = TVAtomic <<< TVPrefix


str :: String -> TabularValue
str = TVAtomic <<< TVString

num :: Number -> TabularValue
num = TVAtomic <<< TVNumber

bool :: Boolean -> TabularValue
bool = TVAtomic <<< TVBoolean

time :: STimeRec -> TabularValue
time = TVAtomic <<< TVTime

date :: SDate -> TabularValue
date = TVAtomic <<< TVDate

dateTime :: SDate -> STimeRec -> TabularValue
dateTime d = TVAtomic <<< TVDateTime d

timeRange :: { from :: STimeRec, to :: STimeRec } -> TabularValue
timeRange = TVAtomic <<< TVTimeRange

dateRange :: { from :: SDate, to :: SDate } -> TabularValue
dateRange = TVAtomic <<< TVDateRange