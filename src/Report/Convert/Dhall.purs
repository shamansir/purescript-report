module Report.Convert.Dhall where

import Report (Report)
import Report.Convert.Generic (class ToExport, IncludeRule) as Report

import Report.Convert.Dhall.Export as Export


toDhall
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toDhall inclRule =
    Export.toDhall @x @subj_id @subj_tag @item_tag inclRule