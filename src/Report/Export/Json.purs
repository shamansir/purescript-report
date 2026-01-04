module Report.Export.Json where

import Prelude

import Yoga.JSON (class WriteForeign, writePrettyJSON)

import Report (Report)
import Report.Export.Generic (class ToExport, toExport) as Report


toJson
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report subj group item
    -> String
toJson =
    Report.toExport @x @subj_id @subj_tag @item_tag >>> writePrettyJSON 4