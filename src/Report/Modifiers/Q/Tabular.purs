module Report.Modifiers.Q.Tabular
    ( module TV
    , time_rng
    , date_rng
    ) where

import Prelude

import Report.Modifiers.Tabular.TabularValue (str, num, bool, time, date, timeRange, dateRange, suf, pref, progress) as TV


time_rng = TV.timeRange
date_rng = TV.dateRange