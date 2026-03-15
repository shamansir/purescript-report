module Report.Decorators.Q.Prefix where

import Prelude

import Report.Core as CT
import Report.Decorators.Task (TaskP(..)) as T
import Report.Decorators.Tags (Tags(..))
import Report.Decorators.Rating (Rating)
import Report.Decorators.Rating (from) as Rating
import Report.Decorators.Priority (Priority(..))
import Report.GroupPath (GroupPath)
import Report.Decorator (Decorator(..), Key(..))


task_todo :: Decorator
task_todo = PTask T.TTodo


task_done :: Decorator
task_done = PTask T.TDone


task_doing :: Decorator
task_doing = PTask T.TDoing


task_wait :: Decorator
task_wait = PTask T.TWait


task_canceled :: Decorator
task_canceled = PTask T.TCanceled


qrating :: { max :: Int, value :: Number } -> Decorator
qrating = PRating <<< Rating.from


rating :: Rating -> Decorator
rating = PRating


priotity :: Priority -> Decorator
priotity = PPriority


r0of5 :: Decorator
r0of5 = qrating { max: 5, value: 0.0 }
r1of5 :: Decorator
r1of5 = qrating { max: 5, value: 1.0 }
r2of5 :: Decorator
r2of5 = qrating { max: 5, value: 2.0 }
r3of5 :: Decorator
r3of5 = qrating { max: 5, value: 3.0 }
r4of5 :: Decorator
r4of5 = qrating { max: 5, value: 4.0 }
r5of5 :: Decorator
r5of5 = qrating { max: 5, value: 5.0 }