module Report.Convert.Org where

import Yoga.JSON (class ReadForeign)

import Report (Report)
import Report.Convert.Generic (class ToExport, class ToImport, IncludeRule) as Report
import Report.Convert.Types (ImportError)
import Data.Either (Either)

import Report.Convert.Org.Export as Export
import Report.Convert.Org.Import as Import


toOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toOrg inclRule =
    Export.toOrg @x @subj_id @subj_tag @item_tag inclRule


fromOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromOrg =
    Import.fromOrg @x @subj_id @subj_tag @item_tag
