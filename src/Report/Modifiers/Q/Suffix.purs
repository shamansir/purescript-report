module Report.Modifiers.Q.Suffix where

import Prelude

import Report.Core as CT
import Report.Modifiers.Task (TaskP(..)) as T
import Report.Modifiers.Progress as P
import Report.Modifiers.Tags (Tags(..))
import Report.GroupPath (GroupPath)
import Report.Suffix (Suffix(..))



text :: forall t. String -> Suffix t
text = SProgress <<< P.PText


num :: forall t. Number -> Suffix t
num = SProgress <<< P.PNumber


int :: forall t. Int -> Suffix t
int = SProgress <<< P.PInt


done :: forall t. Suffix t
done = SProgress $ P.ToComplete { done : true }


none :: forall t. Suffix t
none = SProgress $ P.ToComplete { done : false }


todo :: forall t. Suffix t
todo = none


got_i :: forall t. Int -> Int -> Suffix t
got_i got total = SProgress $ P.ToGetI { got, total }


got_n :: forall t. Number -> Number -> Suffix t
got_n got total = SProgress $ P.ToGetN { got, total }


pct_i :: forall t. Int -> Suffix t
pct_i = SProgress <<< P.PercentI


pct_n :: forall t. Number -> Suffix t
pct_n = SProgress <<< P.PercentN


data Sign
    = Positive
    | Negative


_ston :: Sign -> Int
_ston = case _ of
    Positive -> 1
    Negative -> -1


pct_sn :: forall t. Sign -> Number -> Suffix t
pct_sn sgn n = SProgress $ P.PercentSign { sign : _ston sgn, pct : n }


newtype Measure = Measure String
newtype Per = Per String


meas_i :: forall t. Int -> Measure -> Suffix t
meas_i n (Measure u) = SProgress $ P.MeasuredI { amount : n, measure : u }


meas_n :: forall t. Number -> Measure -> Suffix t
meas_n n (Measure u) = SProgress $ P.MeasuredN { amount : n, measure : u }


meas_sn :: forall t. Sign -> Number -> Measure -> Suffix t
meas_sn sgn n (Measure u) = SProgress $ P.MeasuredSign { sign : _ston sgn, amount : n, measure : u }


per_i :: forall t. Int -> Per -> Suffix t
per_i n (Per u) = SProgress $ P.PerI { amount : n, per : u }


per_n :: forall t. Number -> Per -> Suffix t
per_n n (Per u) = SProgress $ P.PerN { amount : n, per : u }


on_date :: forall t. CT.SDateRec -> Suffix t
on_date = SProgress <<< P.OnDate <<< CT.dateFromRec


on_time :: forall t. CT.STimeRec -> Suffix t
on_time = SProgress <<< P.OnTime


rng_i :: forall t. Int -> Int -> Suffix t
rng_i from to = SProgress $ P.RangeI { from, to }


rng_n :: forall t. Number -> Number -> Suffix t
rng_n from to = SProgress $ P.RangeN { from, to }


task_todo :: forall t. Suffix t
task_todo = SProgress $ P.Task T.TTodo


task_done :: forall t. Suffix t
task_done = SProgress $ P.Task T.TDone


task_doing :: forall t. Suffix t
task_doing = SProgress $ P.Task T.TDoing


task_wait :: forall t. Suffix t
task_wait = SProgress $ P.Task T.TWait


task_canceled :: forall t. Suffix t
task_canceled = SProgress $ P.Task T.TCanceled


task_now :: forall t. Suffix t
task_now = SProgress $ P.Task T.TNow


task_later :: forall t. Suffix t
task_later = SProgress $ P.Task T.TLater


levels_n :: forall t. P.LevelsN -> Suffix t
levels_n = SProgress <<< P.LevelsN


levels_i :: forall t. P.LevelsI -> Suffix t
levels_i = SProgress <<< P.LevelsI


levels_o :: forall t. P.LevelsO -> Suffix t
levels_o = SProgress <<< P.LevelsO


levels_s :: forall t. P.LevelsS -> Suffix t
levels_s = SProgress <<< P.LevelsS


levels_e :: forall t. P.LevelsE -> Suffix t
levels_e = SProgress <<< P.LevelsE


levels_c :: forall t. P.LevelsC -> Suffix t
levels_c = SProgress <<< P.LevelsC


levels_p :: forall t. P.LevelsP -> Suffix t
levels_p = SProgress <<< P.LevelsP


rel_time :: forall t. P.Relation -> CT.STimeRec -> Suffix t
rel_time rel timeRec = SProgress $ P.RelTime rel timeRec


earnedAt :: forall t. CT.SDateRec -> Suffix t
earnedAt = SEarnedAt <<< CT.dateFromRec


details :: forall t. String -> Suffix t
details = SDescription


self_ref :: forall t. GroupPath -> Suffix t
self_ref = SReference


tags :: forall t. Array t -> Suffix t
tags = STags <<< Tags


tag :: forall t. t -> Suffix t
tag = tags <<< pure