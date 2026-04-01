module Report.Web.Decorators.Task where

import Prelude

import Report.Decorators.Task (TaskP(..)) as S

import Report.Web.Helpers
import Report.Web.Decorators.Types


taskColor :: S.TaskP -> String
taskColor = taskVState >>> case _ of
    Complete -> progressBarCompleteColor
    _ -> progressBarIncompleteColor -- TODO: implement other colors


taskTextColor :: S.TaskP -> String
taskTextColor = taskVState >>> case _ of
    Complete -> completeColor
    _ -> incompleteColor -- TODO: implement other colors

taskVState :: S.TaskP -> ProgressVState
taskVState = case _ of
    S.TDone -> Complete
    _ -> Incomplete

qtaskCheckbox :: forall w i. S.TaskP -> H w i
qtaskCheckbox =
    qcheckbox taskColor $ const ""
    -- [ HH.text $ if done then "" {-"●" -} else "" ]
