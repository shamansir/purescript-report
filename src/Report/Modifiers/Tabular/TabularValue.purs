module Report.Modifiers.Tabular.TabularValue where

import Prelude

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