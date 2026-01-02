module Report.Export.Json where

import Prelude

import Yoga.JSON (class WriteForeign, writePrettyJSON)

import Report (Report)
import Report.Class (class IsGroup, class IsItem, class IsSubject, class IsTag)
import Report.Modifiers.Class.ValueModify (class EncodableKey)
import Report.Export.Generic (toExport) as Report


toJson
    :: forall @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => EncodableKey subj_id
    => WriteForeign item_tag
    => IsTag subj_tag
    => IsItem item_tag item
    => IsGroup group
    => IsSubject subj_id subj_tag subj
    => Report subj group item
    -> String
toJson =
    Report.toExport @subj_id @subj_tag @item_tag >>> writePrettyJSON 4