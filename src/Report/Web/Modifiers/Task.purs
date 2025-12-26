module Report.Web.Modifiers.Task where

import Prelude

import Report.Modifiers.Task (TaskP(..)) as S

import Report.Web.Helpers


taskColor :: S.TaskP -> String
taskColor = case _ of
    S.TDone -> progressBarCompleteColor
    _ -> progressBarIncompleteColor -- TODO: implement other colors

taskTextColor :: S.TaskP -> String
taskTextColor = case _ of
    S.TDone -> completeColor
    _ -> incompleteColor -- TODO: implement other colors

qtaskCheckbox :: forall w i. S.TaskP -> H w i
qtaskCheckbox =
    qcheckbox taskColor $ const ""
    -- [ HH.text $ if done then "" {-"●" -} else "" ]
