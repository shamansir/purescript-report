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


task_todo :: forall t. Decorator t
task_todo = PTask T.TTodo


task_done :: forall t. Decorator t
task_done = PTask T.TDone


task_doing :: forall t. Decorator t
task_doing = PTask T.TDoing


task_wait :: forall t. Decorator t
task_wait = PTask T.TWait


task_canceled :: forall t. Decorator t
task_canceled = PTask T.TCanceled


qrating :: forall t. { max :: Int, value :: Number } -> Decorator t
qrating = PRating <<< Rating.from


rating :: forall t. Rating -> Decorator t
rating = PRating


priotity :: forall t. Priority -> Decorator t
priotity = PPriority


r0of5 :: forall t. Decorator t
r0of5 = qrating { max: 5, value: 0.0 }
r1of5 :: forall t. Decorator t
r1of5 = qrating { max: 5, value: 1.0 }
r2of5 :: forall t. Decorator t
r2of5 = qrating { max: 5, value: 2.0 }
r3of5 :: forall t. Decorator t
r3of5 = qrating { max: 5, value: 3.0 }
r4of5 :: forall t. Decorator t
r4of5 = qrating { max: 5, value: 4.0 }
r5of5 :: forall t. Decorator t
r5of5 = qrating { max: 5, value: 5.0 }