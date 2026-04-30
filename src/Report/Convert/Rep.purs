module Report.Convert.Rep where

import Data.Either (Either)

import Report (Report)
import Report.Convert.Types (ImportError)
import Report.Convert.Generic (class ToExport, IncludeRule, class ToImport) as Report

import Report.Convert.Rep.Export as Export
import Report.Convert.Rep.Import as Import


toRep
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toRep inclRule =
    Export.toRep @x @subj_id @subj_tag @item_tag inclRule

{-
fromRep :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToImport subj_id subj_tag item_tag subj group item x
    => String
    -> Either ImportError (Report subj group item)
fromRep =
    Import.fromRep @x @subj_id @subj_tag @item_tag
-}