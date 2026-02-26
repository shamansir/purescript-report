module Report.Decorators.Q.Suffix where

import Prelude

import Report.Core as CT
import Report.Decorators.Task (TaskP(..)) as T
import Report.Decorators.Progress as P
import Report.Decorators.Tags (Tags(..))
import Report.GroupPath (GroupPath)
import Report.Decorator (Decorator(..))



text :: forall t. String -> Decorator t
text = SProgress <<< P.PText


num :: forall t. Number -> Decorator t
num = SProgress <<< P.PNumber


int :: forall t. Int -> Decorator t
int = SProgress <<< P.PInt


done :: forall t. Decorator t
done = SProgress $ P.ToComplete { done : true }


none :: forall t. Decorator t
none = SProgress $ P.ToComplete { done : false }


todo :: forall t. Decorator t
todo = none


got_i :: forall t. Int -> Int -> Decorator t
got_i got total = SProgress $ P.ToGetI { got, total }


got_n :: forall t. Number -> Number -> Decorator t
got_n got total = SProgress $ P.ToGetN { got, total }


pct_i :: forall t. Int -> Decorator t
pct_i = SProgress <<< P.PercentI


pct_n :: forall t. Number -> Decorator t
pct_n = SProgress <<< P.PercentN


data Sign
    = Positive
    | Negative


_ston :: Sign -> Int
_ston = case _ of
    Positive -> 1
    Negative -> -1


pct_sn :: forall t. Sign -> Number -> Decorator t
pct_sn sgn n = SProgress $ P.PercentSign { sign : _ston sgn, pct : n }


newtype Measure = Measure String
newtype Per = Per String


meas_i :: forall t. Int -> Measure -> Decorator t
meas_i n (Measure u) = SProgress $ P.MeasuredI { amount : n, measure : u }


meas_n :: forall t. Number -> Measure -> Decorator t
meas_n n (Measure u) = SProgress $ P.MeasuredN { amount : n, measure : u }


meas_sn :: forall t. Sign -> Number -> Measure -> Decorator t
meas_sn sgn n (Measure u) = SProgress $ P.MeasuredSign { sign : _ston sgn, amount : n, measure : u }


per_i :: forall t. Int -> Per -> Decorator t
per_i n (Per u) = SProgress $ P.PerI { amount : n, per : u }


per_n :: forall t. Number -> Per -> Decorator t
per_n n (Per u) = SProgress $ P.PerN { amount : n, per : u }


on_date :: forall t. CT.SDateRec -> Decorator t
on_date = SProgress <<< P.OnDate <<< CT.dateFromRec


on_time :: forall t. CT.STimeRec -> Decorator t
on_time = SProgress <<< P.OnTime


rng_i :: forall t. Int -> Int -> Decorator t
rng_i from to = SProgress $ P.RangeI { from, to }


rng_n :: forall t. Number -> Number -> Decorator t
rng_n from to = SProgress $ P.RangeN { from, to }


task_todo :: forall t. Decorator t
task_todo = SProgress $ P.Task T.TTodo


task_done :: forall t. Decorator t
task_done = SProgress $ P.Task T.TDone


task_doing :: forall t. Decorator t
task_doing = SProgress $ P.Task T.TDoing


task_wait :: forall t. Decorator t
task_wait = SProgress $ P.Task T.TWait


task_canceled :: forall t. Decorator t
task_canceled = SProgress $ P.Task T.TCanceled


task_now :: forall t. Decorator t
task_now = SProgress $ P.Task T.TNow


task_later :: forall t. Decorator t
task_later = SProgress $ P.Task T.TLater


levels_n :: forall t. P.LevelsN -> Decorator t
levels_n = SProgress <<< P.LevelsN


levels_i :: forall t. P.LevelsI -> Decorator t
levels_i = SProgress <<< P.LevelsI


levels_o :: forall t. P.LevelsO -> Decorator t
levels_o = SProgress <<< P.LevelsO


levels_s :: forall t. P.LevelsS -> Decorator t
levels_s = SProgress <<< P.LevelsS


levels_e :: forall t. P.LevelsE -> Decorator t
levels_e = SProgress <<< P.LevelsE


levels_c :: forall t. P.LevelsC -> Decorator t
levels_c = SProgress <<< P.LevelsC


levels_p :: forall t. P.LevelsP -> Decorator t
levels_p = SProgress <<< P.LevelsP


rel_time :: forall t. P.Relation -> CT.STimeRec -> Decorator t
rel_time rel timeRec = SProgress $ P.RelTime rel timeRec


earnedAt :: forall t. CT.SDateRec -> Decorator t
earnedAt = SEarnedAt <<< CT.dateFromRec


details :: forall t. String -> Decorator t
details = SDescription


self_ref :: forall t. GroupPath -> Decorator t
self_ref = SReference


tags :: forall t. Array t -> Decorator t
tags = STags <<< Tags


tag :: forall t. t -> Decorator t
tag = tags <<< pure