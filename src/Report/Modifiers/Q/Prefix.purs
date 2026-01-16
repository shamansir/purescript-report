module Report.Modifiers.Q.Prefix where

import Prelude

import Report.Core as CT
import Report.Modifiers.Task (TaskP(..)) as T
import Report.Modifiers.Tags (Tags(..))
import Report.Modifiers.Rating (Rating)
import Report.Modifiers.Rating (from) as Rating
import Report.Modifiers.Priority (Priority(..))
import Report.GroupPath (GroupPath)
import Report.Prefix (Prefix(..), Key(..))


task_todo :: Prefix
task_todo = PTask T.TTodo


task_done :: Prefix
task_done = PTask T.TDone


task_doing :: Prefix
task_doing = PTask T.TDoing


task_wait :: Prefix
task_wait = PTask T.TWait


task_canceled :: Prefix
task_canceled = PTask T.TCanceled


qrating :: { max :: Int, value :: Number } -> Prefix
qrating = PRating <<< Rating.from


rating :: Rating -> Prefix
rating = PRating


priotity :: Priority -> Prefix
priotity = PPriority


r0of5 :: Prefix
r0of5 = qrating { max: 5, value: 0.0 }
r1of5 :: Prefix
r1of5 = qrating { max: 5, value: 1.0 }
r2of5 :: Prefix
r2of5 = qrating { max: 5, value: 2.0 }
r3of5 :: Prefix
r3of5 = qrating { max: 5, value: 3.0 }
r4of5 :: Prefix
r4of5 = qrating { max: 5, value: 4.0 }
r5of5 :: Prefix
r5of5 = qrating { max: 5, value: 5.0 }