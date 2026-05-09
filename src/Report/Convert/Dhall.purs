module Report.Convert.Dhall where


import Data.Either (Either)

import Report (Report)
import Report.Convert.Types (ImportError)
import Report.Convert.Generic (class ToExport, class ToImport, IncludeRule) as Report

import Report.Convert.Dhall.Export as Export
import Report.Convert.Dhall.Import as Import


toDhall
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toDhall inclRule =
    Export.toDhall @x @subj_id @subj_tag @item_tag inclRule


fromDhall :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromDhall =
    Import.fromDhall @x @subj_id @subj_tag @item_tag