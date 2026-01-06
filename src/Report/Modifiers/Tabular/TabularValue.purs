module Report.Modifiers.Tabular.TabularValue where

import Prelude

import Foreign (Foreign, F)
import Foreign (fail, ForeignError(..)) as F

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl, readImpl)

import Report.Core (SDate, SDateRec, STimeRec)
import Report.Prefix (Prefix)
import Report.Suffix (Suffix(..))
import Report.Modifiers.Progress (Progress) as P
import Report.Modifiers (class IsModifier)
import Report.Modifiers.Convert.Tagged



data TabularValue
    = TVString String
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


newtype TVTag = TVTag String
derive instance Newtype TVTag _
derive newtype instance ReadForeign  TVTag
derive newtype instance WriteForeign TVTag


instance IsModifier TVTag TabularValue where
    modifierKey = tagOf


instance EncodableKey TVTag where
    encodeKey (TVTag str) = str
    decodeKey str = Just $ TVTag str


instance DecodeTagged TVTag TabularValue where
    toValue = decodeWithKey


{-
type EncodedTV = { tv_tag :: TVTag, tv_v :: Foreign } -- FIXME: use the same system everywhere for modifiers, see `IsModifier`


instance ReadForeign TabularValue where
    readImpl frgn = do
        (encoded :: EncodedTV) <- readImpl frgn
        case encoded.tv_tag of
            TVTag "S"  -> TVString  <$> (readImpl frgn :: F String)
            TVTag "N"  -> TVNumber  <$> (readImpl frgn :: F Number)
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
            _          -> fail $ ForeignError "Unknown TabularValue tag"


instance WriteForeign TabularValue where
    writeImpl = case _ of
        TVString str -> encodeTagged (TVTag "S") str
        TVNumber num -> encodeTagged (TVTag "N") num
        TVBoolean bool -> encodeTagged (TVTag "B") bool
        TVTime timeRec -> encodeTagged (TVTag "T") timeRec
        TVDate sdate -> encodeTagged (TVTag "D") sdate
        TVDateTime sdate stimeRec -> encodeTagged (TVTag "DT") { date: sdate, time: stimeRec }
        TVTimeRange range -> encodeTagged (TVTag "TR") range
        TVDateRange range -> encodeTagged (TVTag "DR") range
        TVDateTimeRange range -> encodeTagged (TVTag "DTR") range
        TVPrefix prefix -> encodeTagged (TVTag "PX") prefix
        TVSuffix suffix -> encodeTagged (TVTag "SX") suffix

        where
            encodeTagged :: forall a. WriteForeign a => TVTag -> a -> Foreign
            encodeTagged tag v = writeImpl ({ tv_tag : tag, tv_v : writeImpl v } :: EncodedTV)



-}
tagOf :: TabularValue -> TVTag
tagOf = TVTag <<< case _ of
    TVString _ -> "S"
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


decodeWithKey :: TVTag -> Foreign -> F TabularValue
decodeWithKey key frgn = case key of
    TVTag "S"  -> TVString  <$> (readImpl frgn :: F String)
    TVTag "N"  -> TVNumber  <$> (readImpl frgn :: F Number)
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
    _          -> F.fail $ F.ForeignError "Unknown TabularValue tag"



instance ReadForeign TabularValue where
    readImpl frgn = decodeTagged @TVTag =<< (readImpl frgn :: F JsonTM)


instance WriteForeign TabularValue where
    writeImpl prefix = writeImpl $ encodeTagged @TVTag prefix


progress :: P.Progress -> TabularValue
progress = TVSuffix <<< SProgress


str :: String -> TabularValue
str = TVString

num :: Number -> TabularValue
num = TVNumber

bool :: Boolean -> TabularValue
bool = TVBoolean

time :: STimeRec -> TabularValue
time = TVTime

date :: SDate -> TabularValue
date = TVDate

dateTime :: SDate -> STimeRec -> TabularValue
dateTime = TVDateTime

timeRange :: { from :: STimeRec, to :: STimeRec } -> TabularValue
timeRange = TVTimeRange

dateRange :: { from :: SDate, to :: SDate } -> TabularValue
dateRange = TVDateRange