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
import Report.Convert.Keyed



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


instance Keyed TVTag TabularValue where
    keyOf = tagOf


instance EncodableKey TVTag where
    encodeKey (TVTag str) = str
    decodeKey str = Just $ TVTag str


instance DecodeKeyed TVTag TabularValue where
    toValue = decodeWithKey


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
    readImpl frgn = decodeKeyed @TVTag =<< (readImpl frgn :: F JsonTM)


instance WriteForeign TabularValue where
    writeImpl tabular = writeImpl $ encodeKeyed @TVTag tabular


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