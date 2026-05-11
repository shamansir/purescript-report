module Report.Convert.Smos where

import Prelude

import Data.Either (Either)

import Report (Report)
import Report.Convert.Types (ImportError)
import Report.Convert.Generic (class ToExport, class ToImport, IncludeRule) as Report
import Report.Convert.Smos.Import (fromSmos) as Report
import Report.Convert.Smos.Export (toSmos) as Report


fromSmos :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromSmos =
    Report.fromSmos @x @subj_id @subj_tag @item_tag


toSmos
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toSmos inclRule =
    Report.toSmos @x @subj_id @subj_tag @item_tag inclRule
