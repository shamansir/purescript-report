module Report.Modifiers.Q.Prefix where

import Prelude

import Report.Core as CT
import Report.Modifiers.Task (TaskP(..)) as T
import Report.Modifiers.Tags (Tags(..))
import Report.Modifiers.Rating (Rating)
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


rating :: Rating -> Prefix
rating = PRating


priotity :: Priority -> Prefix
priotity = PPriority