module Report.Convert.Json where

import Prelude

import Data.Either (Either)

import Report (Report)
import Report.Convert.Types (ImportError)
import Report.Convert.Generic (class ToExport, class ToImport, IncludeRule) as Report
import Report.Convert.Json.Import (fromJson) as Report
import Report.Convert.Json.Export (toJson) as Report


fromJson :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromJson =
    Report.fromJson @x @subj_id @subj_tag @item_tag


toJson
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toJson inclRule =
    Report.toJson @x @subj_id @subj_tag @item_tag inclRule
