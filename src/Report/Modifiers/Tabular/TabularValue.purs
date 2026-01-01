module Report.Modifiers.Tabular.TabularValue where

import Prelude


import Yoga.JSON (class WriteForeign, writeImpl)

import Report.Core (SDate, SDateRec, STimeRec)


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


instance WriteForeign TabularValue where -- FIXME: needs key for the value type
    writeImpl = case _ of
         TVString str -> writeImpl str
         TVNumber num -> writeImpl num
         TVBoolean bool -> writeImpl bool
         TVTime timeRec -> writeImpl timeRec
         TVDate sdate -> writeImpl sdate
         TVDateTime sdate stimeRec -> writeImpl { date: sdate, time: stimeRec }
         TVTimeRange range -> writeImpl range
         TVDateRange range -> writeImpl range
         TVDateTimeRange range -> writeImpl range