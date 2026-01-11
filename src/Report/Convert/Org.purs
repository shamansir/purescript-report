module Report.Convert.Org where

import Yoga.JSON (class ReadForeign)

import Report (Report)
import Report.Class (class IsTag)
import Report.Convert.Generic (class ToExport, IncludeRule) as Report

import Report.Convert.Org.Export as Export


toOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => ReadForeign item_tag
    => IsTag item_tag
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toOrg inclRule =
    Export.toOrg @x @subj_id @subj_tag @item_tag inclRule