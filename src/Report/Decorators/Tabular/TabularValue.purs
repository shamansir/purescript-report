module Report.Decorators.Tabular.TabularValue where

import Prelude

import Foreign (Foreign, F)
import Foreign (fail, ForeignError(..)) as F

import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

import Report.Core (Year, SDate, STimeRec)
import Report.AtomicValue (AtomicValue(..))
import Report.Decorator (Decorator(..), Key, keyOf) as D
import Report.Decorators.Tags (RawTags)
import Report.Decorators.Progress (Progress(..)) as P
import Report.Decorators.Tabular.Logbook (Logbook)
import Report.GroupPath (toChain) as GP
import Report.Convert.Keyed as CK
import Report.Tabular (class ToTabularValue, Tabular)


data TabularValue
    = TVAtomic AtomicValue
    | TVValues (Array AtomicValue)
    | TVValuesNest (Array (Array AtomicValue))
    | TVTabulars (Array (Tabular AtomicValue))
    | TVPair TabularValue TabularValue
    | TVTabularsNest
        { direct :: Array (Tabular AtomicValue)
        , parts :: Array (AtomicValue /\ Array (Tabular AtomicValue))
        }
    | TVLogbook Logbook
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


instance ToTabularValue String TabularValue where toTV = Just <<< TVAtomic <<< PText
instance ToTabularValue Int TabularValue where toTV = Just <<< TVAtomic <<< PInt
instance ToTabularValue Year TabularValue where toTV = Just <<< TVAtomic <<< PYear <<< unwrap
instance ToTabularValue Number TabularValue where toTV = Just <<< TVAtomic <<< PNumber
instance ToTabularValue (Maybe Int) TabularValue where toTV = map (TVAtomic <<< PInt)
instance ToTabularValue (Maybe Number) TabularValue where toTV = map (TVAtomic <<< PNumber)
instance ToTabularValue (Maybe String) TabularValue where toTV = map (TVAtomic <<< PText)
instance ToTabularValue (Maybe Year) TabularValue where toTV = map (TVAtomic <<< PInt <<< unwrap)



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



instance CK.Keyed TVTag TabularValue where
    keyOf = tagOf


instance CK.Keyed TVTag AtomicValue where
    keyOf = tagOfAtomic


instance CK.EncodableKey TVTag where
    encodeKey (TVTag str) = str
    decodeKey str = Just $ TVTag str


instance CK.KeyedReadForeign TVTag AtomicValue where
    keyedReadImpl = decodeAtomicWithKey


instance CK.KeyedReadForeign TVTag TabularValue where
    keyedReadImpl = decodeWithKey


{- Tabular encoding of `AtomicValue`: kept the same as it was for `TabularAtomicValue`,
   the values which have no special tabular tag are encoded as decorators (`DX`) -}


toDecorator :: AtomicValue -> D.Decorator
toDecorator = case _ of
    PRating rating -> D.PRating rating
    PPriority priority -> D.PPriority priority
    PTask taskP -> D.PTask taskP
    aval -> D.SProgress $ P.AV aval


fromDecorator :: D.Decorator -> AtomicValue
fromDecorator = case _ of
    D.PRating rating -> PRating rating
    D.PPriority priority -> PPriority priority
    D.PTask taskP -> PTask taskP
    D.SProgress (P.AV aval) -> aval
    D.SProgress (P.LS _) -> Error "levels are not supported in tabular values"
    D.SEarnedAt sdate -> OnDate sdate
    D.SDescription text -> PText text
    D.SReference path -> maybe (Error "empty reference") PPath $ GP.toChain path
    D.SJoinReference path -> maybe (Error "empty reference") PPath $ GP.toChain path


tagOfAtomic :: AtomicValue -> TVTag
tagOfAtomic = TVTag <<< case _ of
    PText _ -> "S"
    PInt _ -> "I"
    PID _ -> "ID"
    PYear _ -> "Y"
    PNumber _ -> "N"
    PBoolean _ -> "B"
    OnTime _ -> "T"
    OnDate _ -> "D"
    OnDateTime _ _ -> "DT"
    TimeRange _ -> "TR"
    DateRange _ -> "DR"
    DateTimeRange _ -> "DTR"
    PRawTags _ -> "TS"
    _ -> "DX"


tagOf :: TabularValue -> TVTag
tagOf = case _ of
    TVAtomic av -> tagOfAtomic av
    TVValues _ -> TVTag "TV"
    TVValuesNest _ -> TVTag "TVN"
    TVTabulars _ -> TVTag "TVT"
    TVTabularsNest _ -> TVTag "TVTN"
    TVPair _ _ -> TVTag "TVP"
    TVLogbook _ -> TVTag "TVLB"
    -- TVNest _ -> "NS"


-- wraps `AtomicValue` to (de)serialize it with the tabular encoding
newtype TabAV = TabAV AtomicValue


instance ReadForeign TabAV where
    readImpl frgn =
        (CK.readImpl' frgn :: F (CK.KeyedValue TVTag Foreign))
            >>= \kv -> TabAV <$> decodeAtomicWithKey (CK.key kv) (CK.value kv)


instance WriteForeign TabAV where
    writeImpl (TabAV aval) = CK.writeImpl' $ CK.make (CK.encodeKey $ tagOfAtomic aval) $ encodeAtomic aval


unTabAV :: TabAV -> AtomicValue
unTabAV (TabAV aval) = aval


decodeWithKey :: TVTag -> Foreign -> F TabularValue
decodeWithKey key frgn = case key of
    TVTag "TV" -> TVValues <$> map unTabAV <$> (readImpl frgn :: F (Array TabAV))
    TVTag "TVN" -> TVValuesNest <$> map (map unTabAV) <$> (readImpl frgn :: F (Array (Array TabAV)))
    TVTag "TVT" -> TVTabulars <$> map (map unTabAV) <$> (readImpl frgn :: F (Array (Tabular TabAV)))
    TVTag "TVTN" ->
        (readImpl frgn :: F { direct :: Array (Tabular TabAV), parts :: Array (TabAV /\ Array (Tabular TabAV)) })
            <#> \{ direct, parts } ->
                TVTabularsNest
                    { direct : map unTabAV <$> direct
                    , parts : (\(av /\ tbs) -> unTabAV av /\ (map unTabAV <$> tbs)) <$> parts
                    }
    TVTag "TVP" -> (Tuple.uncurry TVPair) <$> (readImpl frgn :: F (TabularValue /\ TabularValue))
    TVTag "TVLB" -> TVLogbook <$> (readImpl frgn :: F Logbook)
    _          -> TVAtomic <$> unTabAV <$> (readImpl frgn :: F TabAV)


instance WriteForeign TabularValue where
    writeImpl tabular = CK.writeImpl' $ CK.make tabularKey $ case tabular of
        TVAtomic av -> writeImpl $ TabAV av
        TVValues avs -> writeImpl $ TabAV <$> avs
        TVValuesNest avsn -> writeImpl $ map TabAV <$> avsn
        TVTabulars tbs -> writeImpl $ map TabAV <$> tbs
        TVTabularsNest { direct, parts } ->
            writeImpl
                { direct : map TabAV <$> direct
                , parts : (\(av /\ tbs) -> TabAV av /\ (map TabAV <$> tbs)) <$> parts
                }
        TVPair tA tB -> writeImpl (tA /\ tB)
        TVLogbook logbook -> writeImpl logbook
        where
            tabularKey = CK.encodeKey $ tagOf tabular


decodeAtomicWithKey :: TVTag -> Foreign -> F AtomicValue
decodeAtomicWithKey key frgn = case key of
    TVTag "S"  -> PText    <$> (readImpl frgn :: F String)
    TVTag "N"  -> PNumber  <$> (readImpl frgn :: F Number)
    TVTag "I"  -> PInt     <$> (readImpl frgn :: F Int)
    TVTag "Y"  -> PYear    <$> (readImpl frgn :: F Int)
    TVTag "ID" -> PID      <$> (readImpl frgn :: F Int)
    TVTag "B"  -> PBoolean <$> (readImpl frgn :: F Boolean)
    TVTag "T"  -> OnTime   <$> (readImpl frgn :: F STimeRec)
    TVTag "D"  -> OnDate   <$> (readImpl frgn :: F SDate)
    TVTag "DT" -> do
        rec <- readImpl frgn :: F { date :: SDate, time :: STimeRec }
        pure $ OnDateTime rec.date rec.time
    TVTag "TR" -> TimeRange <$> (readImpl frgn :: F { from :: STimeRec, to :: STimeRec })
    TVTag "DR" -> DateRange <$> (readImpl frgn :: F { from :: SDate, to :: SDate })
    TVTag "DTR" -> do
        rec <- readImpl frgn :: F { from :: { date :: SDate, time :: STimeRec }, to :: { date :: SDate, time :: STimeRec } }
        pure $ DateTimeRange { from: rec.from, to: rec.to }
    TVTag "DX" -> fromDecorator <$> (readImpl frgn :: F D.Decorator)
    TVTag "TS" -> PRawTags <$> (readImpl frgn :: F RawTags)
    _          -> F.fail $ F.ForeignError "Unknown TabularAtomicValue tag"


-- the inner value of the tabular encoding, the tag is written separately
encodeAtomic :: AtomicValue -> Foreign
encodeAtomic = case _ of
    PText s -> writeImpl s
    PNumber n -> writeImpl n
    PInt n -> writeImpl n
    PID n -> writeImpl n
    PYear n -> writeImpl n
    PBoolean b -> writeImpl b
    OnTime t -> writeImpl t
    OnDate d -> writeImpl d
    OnDateTime d t -> writeImpl { date: d, time: t }
    TimeRange tr -> writeImpl tr
    DateRange dr -> writeImpl dr
    DateTimeRange dtr -> writeImpl dtr
    PRawTags tags -> writeImpl tags
    aval -> writeImpl $ toDecorator aval


instance ReadForeign TabularValue where
    readImpl frgn =
        (CK.readImpl' frgn :: F (CK.KeyedValue TVTag Foreign))
            >>= \kv -> decodeWithKey (CK.key kv) (CK.value kv)
        -- B.decodeKeyed @TVTag =<< (readImpl frgn :: F B.JsonTM)


progress :: P.Progress -> TabularValue
progress = case _ of
    P.AV aval -> TVAtomic aval
    P.LS _ -> TVAtomic $ Error "levels are not supported in tabular values"


atomic :: AtomicValue -> TabularValue
atomic = TVAtomic


dec :: D.Decorator -> TabularValue
dec = TVAtomic <<< fromDecorator

str :: String -> TabularValue
str = TVAtomic <<< PText

num :: Number -> TabularValue
num = TVAtomic <<< PNumber

bool :: Boolean -> TabularValue
bool = TVAtomic <<< PBoolean

time :: STimeRec -> TabularValue
time = TVAtomic <<< OnTime

date :: SDate -> TabularValue
date = TVAtomic <<< OnDate

dateTime :: SDate -> STimeRec -> TabularValue
dateTime d = TVAtomic <<< OnDateTime d

timeRange :: { from :: STimeRec, to :: STimeRec } -> TabularValue
timeRange = TVAtomic <<< TimeRange

dateRange :: { from :: SDate, to :: SDate } -> TabularValue
dateRange = TVAtomic <<< DateRange

tags :: RawTags -> TabularValue
tags = TVAtomic <<< PRawTags

logbook :: Logbook -> TabularValue
logbook = TVLogbook


data TabValTypeKey
    = TVTString
    | TVTInt
    | TVTID
    | TVTYear
    | TVTNumber
    | TVTBoolean
    | TVTTime
    | TVTDate
    | TVTDateTime
    | TVTTimeRange
    | TVTDateRange
    | TVTDateTimeRange
    | TVTDecorator D.Key
    | TVTTags


typeKeyOf :: AtomicValue -> TabValTypeKey
typeKeyOf = case _ of
    PText _ -> TVTString
    PInt _ -> TVTInt
    PID _ -> TVTID
    PYear _ -> TVTYear
    PNumber _ -> TVTNumber
    PBoolean _ -> TVTBoolean
    OnTime _ -> TVTTime
    OnDate _ -> TVTDate
    OnDateTime _ _ -> TVTDateTime
    TimeRange _ -> TVTTimeRange
    DateRange _ -> TVTDateRange
    DateTimeRange _ -> TVTDateTimeRange
    PRawTags _ -> TVTTags
    aval -> TVTDecorator $ D.keyOf $ toDecorator aval
