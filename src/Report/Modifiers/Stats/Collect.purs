module Report.Modifiers.Stats.Collect where

import Prelude

import Data.Tuple (snd) as Tuple
import Data.Maybe (fromMaybe)
import Data.Array (filter, length, head) as Array

import Report.Class (class HasSuffixes, i_suffixes)
import Report.Modifiers.Stats (Stats(..))
import Report.Modifiers.Progress (Progress(..), NProgress(..), loadNProgress)
import Report.Suffix (collectProgress) as Suffix


data CollectWhat
    = ItemsCount
    | ItemsProgress -- from suffixes


collectStats :: forall @tag item. HasSuffixes tag item => CollectWhat -> Array item -> Stats
collectStats what flattened =
    case what of
        ItemsProgress ->
            let
                allProgressN = loadNProgress <$> getProgress <$> flattened
                getProgress :: item -> Progress -- FIXME: we only take one progress per item, need to aggregate all tagged progresses
                getProgress = fromMaybe None <<< map Tuple.snd <<< Array.head <<< Suffix.collectProgress <<< i_suffixes @tag
                nonValue = Array.filter (\i -> i /= Skip && i /= StatsValue) allProgressN
                total = Array.length nonValue
                got = Array.length $ Array.filter (_ == Achieved) nonValue
                onTheWay =
                    Array.length
                        $ Array.filter
                            (case _ of
                                OnTheWay _ -> true
                                _ -> false
                            )
                        $ allProgressN
            in
                if (total == 0) then SNotRelevant
                else
                    SWithProgress
                        { total
                        , got
                        , onTheWay
                        }
        ItemsCount ->
            SCount { count : Array.length flattened }