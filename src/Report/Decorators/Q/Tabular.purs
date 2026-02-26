module Report.Decorators.Q.Tabular
    ( module TV
    , time_rng
    , date_rng
    ) where

import Prelude

import Report.Decorators.Tabular.TabularValue (str, num, bool, time, date, timeRange, dateRange, dec, progress) as TV


time_rng = TV.timeRange
date_rng = TV.dateRange