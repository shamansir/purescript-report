module Report.Decorators.Stats.Collect where

import Prelude

import Data.Tuple (snd) as Tuple
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (filter, length, head) as Array

import Report.Class (class HasDecorators, i_decorators)
import Report.Decorators.Stats (Stats(..))
import Report.Decorators.Progress (Progress(..), NProgress(..), loadNProgress)
import Report.Decorator (collectProgress) as Decorator


data CollectWhat
    = ItemsCount
    | ItemsProgress -- from suffixes


collectStats :: forall item. HasDecorators item => CollectWhat -> Array item -> Stats
collectStats what flattened =
    case what of
        ItemsProgress ->
            let
                allProgressN = loadNProgress <$> getProgress <$> flattened
                getProgress :: item -> Progress -- FIXME: we only take one progress per item, need to aggregate all tagged progresses
                getProgress = fromMaybe None <<< map Tuple.snd <<< Array.head <<< Decorator.collectProgress <<< i_decorators
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
                    $ Just allProgressN
        ItemsCount ->
            SCount { count : Array.length flattened }