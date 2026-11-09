module Report.Decorators.Q.Suffix where

import Prelude

import Report.Core as CT
import Report.Decorators.Task (TaskP(..)) as T
import Report.Decorators.Progress (Progress(..), Levels(..)) as P
import Report.AtomicValue (AtomicValue(..)) as P
import Report.Decorators.Levels (LevelsC, LevelsE, LevelsI, LevelsN, LevelsO, LevelsP, LevelsS) as P
import Report.GroupPath (GroupPath)
import Report.Decorator (Decorator(..))



text :: String -> Decorator
text = SProgress <<< P.AV <<< P.PText


num :: Number -> Decorator
num = SProgress <<< P.AV <<< P.PNumber


int :: Int -> Decorator
int = SProgress <<< P.AV <<< P.PInt


done :: Decorator
done = SProgress $ P.AV $ P.ToComplete { done : true }


none :: Decorator
none = SProgress $ P.AV $ P.ToComplete { done : false }


todo :: Decorator
todo = none


got_i :: Int -> Int -> Decorator
got_i got total = SProgress $ P.AV $ P.ToGetI { got, total }


got_n :: Number -> Number -> Decorator
got_n got total = SProgress $ P.AV $ P.ToGetN { got, total }


pct_i :: Int -> Decorator
pct_i = SProgress <<< P.AV <<< P.PercentI


pct_n :: Number -> Decorator
pct_n = SProgress <<< P.AV <<< P.PercentN


data Sign
    = Positive
    | Negative


_ston :: Sign -> Int
_ston = case _ of
    Positive -> 1
    Negative -> -1


pct_sn :: Sign -> Number -> Decorator
pct_sn sgn n = SProgress $ P.AV $ P.PercentSign { sign : _ston sgn, pct : n }


newtype Measure = Measure String
newtype Per = Per String


meas_i :: Int -> Measure -> Decorator
meas_i n (Measure u) = SProgress $ P.AV $ P.MeasuredI { amount : n, measure : u }


meas_n :: Number -> Measure -> Decorator
meas_n n (Measure u) = SProgress $ P.AV $ P.MeasuredN { amount : n, measure : u }


meas_sn :: Sign -> Number -> Measure -> Decorator
meas_sn sgn n (Measure u) = SProgress $ P.AV $ P.MeasuredSign { sign : _ston sgn, amount : n, measure : u }


per_i :: Int -> Per -> Decorator
per_i n (Per u) = SProgress $ P.AV $ P.PerI { amount : n, per : u }


per_n :: Number -> Per -> Decorator
per_n n (Per u) = SProgress $ P.AV $ P.PerN { amount : n, per : u }


on_date :: CT.SDateRec -> Decorator
on_date = SProgress <<< P.AV <<< P.OnDate <<< CT.dateFromRec


on_time :: CT.STimeRec -> Decorator
on_time = SProgress <<< P.AV <<< P.OnTime


rng_i :: Int -> Int -> Decorator
rng_i from to = SProgress $ P.AV $ P.RangeI { from, to }


rng_n :: Number -> Number -> Decorator
rng_n from to = SProgress $ P.AV $ P.RangeN { from, to }


task_todo :: Decorator
task_todo = SProgress $ P.AV $ P.PTask T.TTodo


task_done :: Decorator
task_done = SProgress $ P.AV $ P.PTask T.TDone


task_doing :: Decorator
task_doing = SProgress $ P.AV $ P.PTask T.TDoing


task_wait :: Decorator
task_wait = SProgress $ P.AV $ P.PTask T.TWait


task_canceled :: Decorator
task_canceled = SProgress $ P.AV $ P.PTask T.TCanceled


task_now :: Decorator
task_now = SProgress $ P.AV $ P.PTask T.TNow


task_later :: Decorator
task_later = SProgress $ P.AV $ P.PTask T.TLater


levels_n :: P.LevelsN -> Decorator
levels_n = SProgress <<< P.LS <<< P.LevelsN


levels_i :: P.LevelsI -> Decorator
levels_i = SProgress <<< P.LS <<< P.LevelsI


levels_o :: P.LevelsO -> Decorator
levels_o = SProgress <<< P.LS <<< P.LevelsO


levels_s :: P.LevelsS -> Decorator
levels_s = SProgress <<< P.LS <<< P.LevelsS


levels_e :: P.LevelsE -> Decorator
levels_e = SProgress <<< P.LS <<< P.LevelsE


levels_c :: P.LevelsC -> Decorator
levels_c = SProgress <<< P.LS <<< P.LevelsC


levels_p :: P.LevelsP -> Decorator
levels_p = SProgress <<< P.LS <<< P.LevelsP


rel_time :: CT.Relation -> CT.STimeRec -> Decorator
rel_time rel timeRec = SProgress $ P.AV $ P.RelTime rel timeRec


earnedAt :: CT.SDateRec -> Decorator
earnedAt = SEarnedAt <<< CT.dateFromRec


details :: String -> Decorator
details = SDescription


self_ref :: GroupPath -> Decorator
self_ref = SReference