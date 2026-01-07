module Report.Convert.Json where

import Prelude

import Yoga.JSON (writePrettyJSON)

import Report (Report)
import Report.Convert.Generic (class ToExport, toExport, IncludeRule) as Report


toJson
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toJson inclRule =
    Report.toExport @x @subj_id @subj_tag @item_tag inclRule >>> writePrettyJSON 4