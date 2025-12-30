module Report.Modifiers.Tabular.TabularValue where

import Prelude

import Report.Core (SDateRec, STimeRec)


data TabularValue
   = TVString String
   | TVNumber Number
   | TVBoolean Boolean
   | TVTime STimeRec
   | TVDate SDateRec
   | TVDateTime SDateRec STimeRec
   | TVTimeRange { from :: STimeRec, to :: STimeRec }
   | TVDateRange { from :: SDateRec, to :: SDateRec }
   | TVDateTimeRange
        { from :: { date :: SDateRec, time :: STimeRec }
        , to :: { date :: SDateRec, time :: STimeRec }
        }