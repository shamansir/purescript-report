module Report.Web.Prefix.Task where

import Prelude

import Report.Prefix.Task (TaskP(..)) as S

import Report.Web.Helpers


taskColor = case _ of
    S.TDone -> progressBarCompleteColor
    _ -> progressBarIncompleteColor -- TODO: implement other colors

taskTextColor = case _ of
    S.TDone -> completeColor
    _ -> incompleteColor -- TODO: implement other colors


qtaskCheckbox =
    qcheckbox taskColor $ const ""
    -- [ HH.text $ if done then "" {-"●" -} else "" ]
