module Report.Convert.Text where

import Prelude

import Yoga.JSON (class ReadForeign)

import Report (Report)
import Report.Class (class IsTag)
import Report.Convert.Generic (class ToExport, IncludeRule) as Report

import Report.Convert.Text.Export as Export


toText
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => IsTag item_tag
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toText inclRule =
    Export.toText @x @subj_id @subj_tag @item_tag inclRule
